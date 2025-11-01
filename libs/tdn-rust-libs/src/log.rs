// The log levels aren't always all used, but it's cleaner to define them together.
#![allow(unused)]

use core::ffi::c_char;

type ApiLevel = u8;
type LogCallback = extern "C" fn(ApiLevel, *const c_char, usize, *const c_char, usize);

const SPIDIR_LOG_LEVEL_NONE: ApiLevel = 0;
const SPIDIR_LOG_LEVEL_ERROR: ApiLevel = 1;
const SPIDIR_LOG_LEVEL_WARN: ApiLevel = 2;
const SPIDIR_LOG_LEVEL_INFO: ApiLevel = 3;
const SPIDIR_LOG_LEVEL_DEBUG: ApiLevel = 4;
const SPIDIR_LOG_LEVEL_TRACE: ApiLevel = 5;

mod imp {
    use alloc::string::ToString;
    use core::{
        ffi::c_char,
        mem, ptr,
        sync::atomic::{AtomicPtr, Ordering},
    };

    use log::{Level, LevelFilter, Log};

    use super::*;

    fn log_level_to_api(level: Level) -> ApiLevel {
        match level {
            Level::Error => SPIDIR_LOG_LEVEL_ERROR,
            Level::Warn => SPIDIR_LOG_LEVEL_WARN,
            Level::Info => SPIDIR_LOG_LEVEL_INFO,
            Level::Debug => SPIDIR_LOG_LEVEL_DEBUG,
            Level::Trace => SPIDIR_LOG_LEVEL_TRACE,
        }
    }

    fn log_filter_to_api(level: LevelFilter) -> ApiLevel {
        match level {
            LevelFilter::Off => SPIDIR_LOG_LEVEL_NONE,
            LevelFilter::Error => SPIDIR_LOG_LEVEL_ERROR,
            LevelFilter::Warn => SPIDIR_LOG_LEVEL_WARN,
            LevelFilter::Info => SPIDIR_LOG_LEVEL_INFO,
            LevelFilter::Debug => SPIDIR_LOG_LEVEL_DEBUG,
            LevelFilter::Trace => SPIDIR_LOG_LEVEL_TRACE,
        }
    }

    fn log_filter_from_api(level: ApiLevel) -> LevelFilter {
        match level {
            SPIDIR_LOG_LEVEL_NONE => LevelFilter::Off,
            SPIDIR_LOG_LEVEL_ERROR => LevelFilter::Error,
            SPIDIR_LOG_LEVEL_WARN => LevelFilter::Warn,
            SPIDIR_LOG_LEVEL_INFO => LevelFilter::Info,
            SPIDIR_LOG_LEVEL_DEBUG => LevelFilter::Debug,
            SPIDIR_LOG_LEVEL_TRACE => LevelFilter::Trace,
            _ => panic!("invalid log level {level}"),
        }
    }

    static LOG_CALLBACK: AtomicPtr<()> = AtomicPtr::new(ptr::null_mut());

    struct Logger;

    impl Log for Logger {
        fn enabled(&self, _metadata: &log::Metadata) -> bool {
            true
        }

        fn log(&self, record: &log::Record) {
            let callback = LOG_CALLBACK.load(Ordering::Acquire);
            if !callback.is_null() {
                // Safety: `spidir_log_init` is required to provide a valid pointer.
                let callback: LogCallback = unsafe { mem::transmute(callback) };
                let module = record.module_path().unwrap_or("");
                let message = record.args().to_string();
                callback(
                    log_level_to_api(record.level()),
                    module.as_ptr() as *const c_char,
                    module.len(),
                    message.as_ptr() as *const c_char,
                    message.len(),
                );
            }
        }

        fn flush(&self) {}
    }

    #[unsafe(no_mangle)]
    unsafe extern "C" fn spidir_log_init(callback: LogCallback) {
        static LOGGER: Logger = Logger;
        log::set_logger(&LOGGER).expect("logging already initialized");
        LOG_CALLBACK.store(callback as *mut (), Ordering::Release);
    }

    #[unsafe(no_mangle)]
    unsafe extern "C" fn spidir_log_set_max_level(level: ApiLevel) {
        log::set_max_level(log_filter_from_api(level));
    }

    #[unsafe(no_mangle)]
    unsafe extern "C" fn spidir_log_get_max_level() -> ApiLevel {
        log_filter_to_api(log::max_level())
    }
}