pub struct Sample {
    pub fn sample(): string {
        return "some shit\n";
    }
}

pub fn main() {
    #arr: string[3];

    Sample.sample();
    arr[0] = "bllblblbl";
    arr[1] = Sample.sample();
}