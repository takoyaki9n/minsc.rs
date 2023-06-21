use crate::env::Env;

pub(crate) mod numbers;

pub fn define_procs(env: &Env) {
    numbers::define_procs(env);
}
