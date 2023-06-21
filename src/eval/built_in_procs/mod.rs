use crate::env::Env;

mod numbers;

pub(super) fn define_procs(env: &Env) {
    numbers::define_procs(env);
}
