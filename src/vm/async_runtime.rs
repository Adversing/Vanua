use crate::vm::value::{FutureValue, SimpleFutureValue};
use crate::vm::{ByteCode, FunctionBytecode, Value, VanuaError, VirtualMachine};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tokio::sync::{oneshot, Semaphore};
use tokio::time::{Duration, Instant};
use tokio_util::sync::CancellationToken;

#[allow(dead_code)]
pub struct AsyncRuntime {
    runtime: tokio::runtime::Runtime,
    task_counter: Arc<Mutex<u64>>,
    active_tasks: Arc<Mutex<HashMap<u64, TaskHandle>>>,
    task_semaphore: Arc<Semaphore>,
    cancellation_tokens: Arc<Mutex<HashMap<u64, CancellationToken>>>,
    task_metrics: Arc<Mutex<TaskMetrics>>,
    debug_mode: bool,
}

#[allow(dead_code)]
pub struct TaskHandle {
    pub id: u64,
    pub sender: Option<oneshot::Sender<SimpleFutureValue>>,
    pub receiver: Option<oneshot::Receiver<SimpleFutureValue>>,
    pub start_time: Instant,
    pub function_name: String,
    pub cancellation_token: CancellationToken,
}

#[derive(Debug, Clone)]
pub struct TaskMetrics {
    pub total_tasks_spawned: u64,
    pub total_tasks_completed: u64,
    pub total_tasks_failed: u64,
    pub total_tasks_cancelled: u64,
    pub average_execution_time_ms: f64,
    pub peak_concurrent_tasks: u64,
    pub current_active_tasks: u64,
}

pub struct AsyncExecutionContext {
    pub function_name: String,
    pub parameters: Vec<SimpleFutureValue>,
    pub function_bytecode: FunctionBytecode,
    pub global_bytecode: ByteCode,
    pub user_defined_functions: HashMap<String, FunctionBytecode>,
    pub debug_mode: bool,
}

impl AsyncRuntime {
    pub fn new(debug_mode: bool) -> Result<Self, VanuaError> {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(num_cpus::get())
            .thread_name("vanua-async-worker")
            .thread_stack_size(2 * 1024 * 1024)
            .enable_all()
            .build()
            .map_err(|e| VanuaError::RuntimeError {
                message: format!("Failed to create async runtime: {}", e),
                cause: None,
            })?;

        let max_concurrent_tasks = num_cpus::get() * 4;

        Ok(AsyncRuntime {
            runtime,
            task_counter: Arc::new(Mutex::new(0)),
            active_tasks: Arc::new(Mutex::new(HashMap::new())),
            task_semaphore: Arc::new(Semaphore::new(max_concurrent_tasks)),
            cancellation_tokens: Arc::new(Mutex::new(HashMap::new())),
            task_metrics: Arc::new(Mutex::new(TaskMetrics {
                total_tasks_spawned: 0,
                total_tasks_completed: 0,
                total_tasks_failed: 0,
                total_tasks_cancelled: 0,
                average_execution_time_ms: 0.0,
                peak_concurrent_tasks: 0,
                current_active_tasks: 0,
            })),
            debug_mode,
        })
    }

    pub fn spawn_async_function(
        &self,
        context: AsyncExecutionContext,
    ) -> Result<Arc<Mutex<FutureValue>>, VanuaError> {
        let semaphore = self.task_semaphore.clone();
        let permit = semaphore
            .try_acquire_owned()
            .map_err(|_| VanuaError::RuntimeError {
                message: "Too many concurrent async tasks. Please wait for some to complete."
                    .to_string(),
                cause: None,
            })?;

        let (sender, receiver) = oneshot::channel();

        let task_id = {
            let mut counter = self.task_counter.lock().unwrap();
            *counter += 1;
            *counter
        };

        let cancellation_token = CancellationToken::new();
        {
            let mut tokens = self.cancellation_tokens.lock().unwrap();
            tokens.insert(task_id, cancellation_token.clone());
        }

        {
            let mut metrics = self.task_metrics.lock().unwrap();
            metrics.total_tasks_spawned += 1;
            metrics.current_active_tasks += 1;
            if metrics.current_active_tasks > metrics.peak_concurrent_tasks {
                metrics.peak_concurrent_tasks = metrics.current_active_tasks;
            }
        }

        let future_value = Arc::new(Mutex::new(FutureValue {
            completed: false,
            value: None,
            receiver: None,
            oneshot_receiver: Some(receiver),
        }));

        let future_clone = future_value.clone();
        let metrics_clone = self.task_metrics.clone();
        let tokens_clone = self.cancellation_tokens.clone();
        let _function_name = context.function_name.clone();
        let start_time = Instant::now();

        let runtime_handle = self.runtime.handle().clone();
        runtime_handle.spawn_blocking(move || {
            let execution_result = {
                if cancellation_token.is_cancelled() {
                    SimpleFutureValue::String("Task was cancelled before execution".to_string())
                } else {
                    execute_function_logic_sync(context)
                }
            };

            let execution_time = start_time.elapsed();
            {
                let mut metrics = metrics_clone.lock().unwrap();
                metrics.current_active_tasks -= 1;

                if execution_result.to_string().contains("cancelled") {
                    metrics.total_tasks_cancelled += 1;
                } else if execution_result.to_string().contains("error")
                    || execution_result.to_string().contains("failed")
                {
                    metrics.total_tasks_failed += 1;
                } else {
                    metrics.total_tasks_completed += 1;
                }

                let total_completed = metrics.total_tasks_completed + metrics.total_tasks_failed;
                if total_completed > 0 {
                    metrics.average_execution_time_ms = (metrics.average_execution_time_ms
                        * (total_completed - 1) as f64
                        + execution_time.as_millis() as f64)
                        / total_completed as f64;
                }
            }

            {
                let mut tokens = tokens_clone.lock().unwrap();
                tokens.remove(&task_id);
            }

            let result_clone = execution_result.clone();
            let _ = sender.send(execution_result);

            {
                let mut future = future_clone.lock().unwrap();
                future.completed = true;
                future.value = Some(result_clone);
            }

            drop(permit);
        });

        Ok(future_value)
    }

    /// Block on a future until completion
    pub fn block_on_future(
        &self,
        future: Arc<Mutex<FutureValue>>,
    ) -> Result<SimpleFutureValue, VanuaError> {
        {
            let future_guard = future.lock().unwrap();
            if future_guard.completed {
                if let Some(ref value) = future_guard.value {
                    return Ok(value.clone());
                }
            }
        }

        let receiver = {
            let mut future_guard = future.lock().unwrap();
            future_guard.oneshot_receiver.take()
        };

        if let Some(receiver) = receiver {
            self.runtime.block_on(async {
                match receiver.await {
                    Ok(result) => {
                        {
                            let mut future_guard = future.lock().unwrap();
                            future_guard.value = Some(result.clone());
                            future_guard.completed = true;
                        }
                        Ok(result)
                    }
                    Err(_) => Err(VanuaError::RuntimeError {
                        message: "Async task was cancelled or failed".to_string(),
                        cause: None,
                    }),
                }
            })
        } else {
            Err(VanuaError::RuntimeError {
                message: "Future was not properly initialized".to_string(),
                cause: None,
            })
        }
    }

    /// Cancel all active async tasks
    pub fn cancel_all_tasks(&self) {
        let tokens = self.cancellation_tokens.lock().unwrap();
        for token in tokens.values() {
            token.cancel();
        }
    }

    /// Cancel a specific task by ID
    pub fn cancel_task(&self, task_id: u64) -> bool {
        let tokens = self.cancellation_tokens.lock().unwrap();
        if let Some(token) = tokens.get(&task_id) {
            token.cancel();
            true
        } else {
            false
        }
    }

    /// Get current task metrics for monitoring
    pub fn get_metrics(&self) -> TaskMetrics {
        self.task_metrics.lock().unwrap().clone()
    }

    /// Reset task metrics
    pub fn reset_metrics(&self) {
        let mut metrics = self.task_metrics.lock().unwrap();
        *metrics = TaskMetrics {
            total_tasks_spawned: 0,
            total_tasks_completed: 0,
            total_tasks_failed: 0,
            total_tasks_cancelled: 0,
            average_execution_time_ms: 0.0,
            peak_concurrent_tasks: 0,
            current_active_tasks: metrics.current_active_tasks,
        };
    }

    /// Get the number of available task slots
    pub fn available_task_slots(&self) -> usize {
        self.task_semaphore.available_permits()
    }

    /// Check if the runtime is healthy
    pub fn health_check(&self) -> Result<(), VanuaError> {
        let metrics = self.get_metrics();

        let total_tasks = metrics.total_tasks_completed
            + metrics.total_tasks_failed
            + metrics.total_tasks_cancelled;
        if total_tasks > 10 {
            let failure_rate = (metrics.total_tasks_failed as f64) / (total_tasks as f64);
            if failure_rate > 0.5 {
                return Err(VanuaError::RuntimeError {
                    message: format!("High task failure rate: {:.1}%", failure_rate * 100.0),
                    cause: None,
                });
            }
        }

        if metrics.average_execution_time_ms > 10000.0 {
            return Err(VanuaError::RuntimeError {
                message: format!(
                    "High average execution time: {:.1}ms",
                    metrics.average_execution_time_ms
                ),
                cause: None,
            });
        }

        Ok(())
    }

    /// Wait for all active tasks to complete with timeout
    pub fn wait_for_all_tasks(&self, timeout_ms: u64) -> Result<(), VanuaError> {
        let start_time = Instant::now();
        let timeout_duration = Duration::from_millis(timeout_ms);

        loop {
            let active_count = {
                let metrics = self.task_metrics.lock().unwrap();
                metrics.current_active_tasks
            };

            if active_count == 0 {
                return Ok(());
            }

            if start_time.elapsed() > timeout_duration {
                return Err(VanuaError::RuntimeError {
                    message: format!(
                        "Timeout waiting for {} active tasks to complete after {}ms",
                        active_count, timeout_ms
                    ),
                    cause: None,
                });
            }

            Self::avoid_busy_waiting();
        }
    }

    /// Wait for all active tasks to complete without timeout
    pub fn wait_for_all_tasks_indefinitely(&self) -> Result<(), VanuaError> {
        loop {
            let active_count = {
                let metrics = self.task_metrics.lock().unwrap();
                metrics.current_active_tasks
            };

            if active_count == 0 {
                return Ok(());
            }

            Self::avoid_busy_waiting();
        }
    }

    /// Get the number of currently active tasks
    pub fn get_active_task_count(&self) -> u64 {
        let metrics = self.task_metrics.lock().unwrap();
        metrics.current_active_tasks
    }

    /// Graceful shutdown of the async runtime
    pub fn shutdown(&self) {
        self.cancel_all_tasks();
        let _ = self.wait_for_all_tasks(2000);
    }

    fn avoid_busy_waiting() {
        std::thread::sleep(Duration::from_millis(10));
    }
}

/// Proper cleanup when AsyncRuntime is dropped
impl Drop for AsyncRuntime {
    fn drop(&mut self) {
        self.cancel_all_tasks();
        let _ = self.wait_for_all_tasks(1000);
    }
}

#[allow(dead_code)]
async fn execute_function_logic(context: AsyncExecutionContext) -> SimpleFutureValue {
    let mut async_vm = VirtualMachine::new();
    async_vm.set_stack_limit(10000); // TODO refactor this

    async_vm.set_globals(crate::stdlib::init_stdlib_silent());

    for (name, function_bytecode) in &context.user_defined_functions {
        async_vm
            .globals
            .insert(name.clone(), Value::Function(name.clone()));
        async_vm
            .function_registry
            .insert(name.clone(), function_bytecode.clone());
    }

    let function_bytecode = context.function_bytecode;
    let global_bytecode = context.global_bytecode;
    let parameters = context.parameters;

    if let Err(e) = load_globals_from_bytecode_safe(&mut async_vm, &global_bytecode) {
        return SimpleFutureValue::String(format!("Failed to load globals: {}", e));
    }

    let mut retry_count = 0;
    const MAX_RETRIES: u32 = 3;

    loop {
        let execution_result = execute_function_bytecode_async(
            &mut async_vm,
            &function_bytecode,
            &parameters,
            &context.function_name,
        )
        .await;

        if retry_count < MAX_RETRIES && is_retryable_error(&execution_result) {
            retry_count += 1;

            let delay = Duration::from_millis(100 * (1 << retry_count));
            tokio::time::sleep(delay).await;

            async_vm = VirtualMachine::new();
            async_vm.set_globals(crate::stdlib::init_stdlib_silent());

            for (name, function_bytecode) in &context.user_defined_functions {
                async_vm
                    .globals
                    .insert(name.clone(), Value::Function(name.clone()));
                async_vm
                    .function_registry
                    .insert(name.clone(), function_bytecode.clone());
            }

            if load_globals_from_bytecode_safe(&mut async_vm, &global_bytecode).is_err() {
                break;
            }

            continue;
        }

        return if retry_count > 0 {
            match &execution_result {
                SimpleFutureValue::String(s) if s.contains("error") || s.contains("failed") => {
                    SimpleFutureValue::String(format!(
                        "Failed after {} retries: {}",
                        retry_count, s
                    ))
                }
                _ => execution_result,
            }
        } else {
            execution_result
        };
    }

    SimpleFutureValue::String("Failed to execute after maximum retries".to_string())
}

fn execute_function_logic_sync(context: AsyncExecutionContext) -> SimpleFutureValue {
    let mut async_vm = VirtualMachine::new();
    async_vm.set_stack_limit(10000); // TODO refactor this

    async_vm.set_globals(crate::stdlib::init_stdlib_silent());

    for (name, function_bytecode) in &context.user_defined_functions {
        async_vm
            .globals
            .insert(name.clone(), Value::Function(name.clone()));
        async_vm
            .function_registry
            .insert(name.clone(), function_bytecode.clone());
    }

    let wrapper_function_bytecode = context.function_bytecode;
    let global_bytecode = context.global_bytecode;
    let parameters = context.parameters;

    if context.debug_mode {
        eprintln!(
            "[AsyncRuntime] Executing async body '{}' with {} instructions and {} constants",
            wrapper_function_bytecode.name,
            wrapper_function_bytecode.instructions.len(),
            wrapper_function_bytecode.constants.len()
        );
    }

    let function_bytecode = wrapper_function_bytecode;

    if let Err(e) = load_globals_from_bytecode_safe(&mut async_vm, &global_bytecode) {
        return SimpleFutureValue::String(format!("Failed to load globals: {}", e));
    }

    let mut vm_parameters = Vec::new();
    for param in &parameters {
        vm_parameters.push(param.to_value());
    }

    match async_vm.call_function(&function_bytecode, vm_parameters) {
        Ok(()) => match async_vm.continue_execution() {
            Ok(result) => SimpleFutureValue::from(result),
            Err(e) => SimpleFutureValue::String(format!(
                "[AsyncRuntime] Function '{}' failed with error: {}",
                context.function_name, e
            )),
        },
        Err(e) => SimpleFutureValue::String(format!(
            "[AsyncRuntime] Failed to call function '{}': {}",
            context.function_name, e
        )),
    }
}

/// Execute function bytecode asynchronously
async fn execute_function_bytecode_async(
    vm: &mut VirtualMachine,
    function_bytecode: &FunctionBytecode,
    parameters: &[SimpleFutureValue],
    function_name: &str,
) -> SimpleFutureValue {
    let mut vm_parameters = Vec::new();
    for param in parameters {
        vm_parameters.push(param.to_value());
    }

    let original_stack_size = vm.stack.len();
    for param in vm_parameters {
        vm.stack.push(param);
    }

    let execution_result = async {
        tokio::task::yield_now().await;

        let mut vm_params = Vec::new();
        for param in parameters.iter() {
            vm_params.push(param.to_value());
        }

        let execution_future = async {
            let mut locals = vec![Value::Nil; function_bytecode.local_count];

            for (i, param) in vm_params.iter().enumerate() {
                if i < locals.len() {
                    locals[i] = param.clone();
                }
            }

            let frame = crate::vm::CallFrame {
                function_name: function_bytecode.name.clone(),
                ip: 0,
                slot_base: vm.stack.len(),
                locals,
                bytecode: Some(function_bytecode.clone()),
            };

            vm.frames.push(frame);
            vm.current_frame_idx = vm.frames.len() - 1;

            loop {
                let current_frame_idx = vm.current_frame_idx;

                if current_frame_idx >= vm.frames.len() {
                    break;
                }

                let ip = vm.frames[current_frame_idx].ip;
                let instruction = if let Some(ref bytecode) = vm.frames[current_frame_idx].bytecode
                {
                    if ip >= bytecode.instructions.len() {
                        break;
                    }
                    bytecode.instructions[ip].clone()
                } else {
                    return Err(VanuaError::RuntimeError {
                        message: "No bytecode available for function execution".to_string(),
                        cause: None,
                    });
                };

                vm.frames[current_frame_idx].ip += 1;

                match vm.execute_single_instruction(&instruction, &function_bytecode.constants) {
                    Ok(()) => {}
                    Err(e) => return Err(e),
                }
            }

            Ok(vm.stack.pop().unwrap_or(Value::Nil))
        };

        tokio::time::timeout(Duration::from_secs(30), execution_future)
            .await
            .unwrap_or_else(|_| {
                Err(VanuaError::RuntimeError {
                    message: "Async function execution timed out after 30 seconds".to_string(),
                    cause: None,
                })
            })
    }
    .await;

    if execution_result.is_err() {
        vm.stack.truncate(original_stack_size);
    }

    match execution_result {
        Ok(value) => SimpleFutureValue::from(value),
        Err(error) => {
            println!(
                "[AsyncRuntime] Function '{}' failed with error: {}",
                function_name, error
            );
            SimpleFutureValue::String(format!(
                "Async execution failed in function '{}': {}",
                function_bytecode.name, error
            ))
        }
    }
}

impl SimpleFutureValue {
    pub fn from(value: Value) -> Self {
        match value {
            Value::Int(i) => SimpleFutureValue::Int(i),
            Value::Float(f) => SimpleFutureValue::Float(f),
            Value::Bool(b) => SimpleFutureValue::Bool(b),
            Value::Char(c) => SimpleFutureValue::Char(c),
            Value::String(s) => SimpleFutureValue::String(s),
            _ => SimpleFutureValue::Nil,
        }
    }
}

fn load_globals_from_bytecode_safe(
    vm: &mut VirtualMachine,
    bytecode: &ByteCode,
) -> Result<(), String> {
    for (index, constant) in bytecode.constants.iter().enumerate() {
        let result: Result<(), String> = match constant {
            crate::vm::Constant::String(s) => {
                vm.globals
                    .insert(format!("__const_{}", index), Value::String(s.clone()));
                Ok(())
            }
            crate::vm::Constant::Int(i) => {
                vm.globals
                    .insert(format!("__const_{}", index), Value::Int(i.clone()));
                Ok(())
            }
            crate::vm::Constant::Float(f) => {
                vm.globals
                    .insert(format!("__const_{}", index), Value::Float(f.clone()));
                Ok(())
            }
            crate::vm::Constant::Bool(b) => {
                vm.globals
                    .insert(format!("__const_{}", index), Value::Bool(b.clone()));
                Ok(())
            }
            crate::vm::Constant::Char(c) => {
                vm.globals
                    .insert(format!("__const_{}", index), Value::Char(c.clone()));
                Ok(())
            }
            crate::vm::Constant::Function(_) => Ok(()),
            crate::vm::Constant::Native(name) => {
                vm.globals
                    .insert(format!("__native_{}", index), Value::String(name.clone()));
                Ok(())
            }
            crate::vm::Constant::Null => {
                vm.globals.insert(format!("__const_{}", index), Value::Nil);
                Ok(())
            }
        };

        if result.is_err() {
            return Err(format!("Failed to load constant at index {}", index));
        }
    }

    for (_index, constant) in bytecode.constants.iter().enumerate() {
        if let crate::vm::Constant::Function(function) = constant {
            vm.globals.insert(
                function.name.clone(),
                Value::Function(function.name.clone()),
            );

            vm.register_function(function.name.clone(), function.clone());
        }
    }

    Ok(())
}

#[allow(dead_code)]
fn is_retryable_error(result: &SimpleFutureValue) -> bool {
    match result {
        SimpleFutureValue::String(s) => {
            // TODO refactor this
            s.contains("timeout")
                || s.contains("network")
                || s.contains("temporary")
                || s.contains("resource temporarily unavailable")
        }
        _ => false,
    }
}
