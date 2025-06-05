use rand::{Rng, distributions::Alphanumeric};

pub fn generate_random_hex() -> String {
    let rng = rand::thread_rng();
    let rand_string: String = rng.sample_iter(&Alphanumeric).take(10).map(char::from).collect();
    rand_string
}
