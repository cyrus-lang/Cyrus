extern fn pow(base: double, exp: double): double as local_pow;
extern fn sqrt(x: double): double as local_sqrt;
extern fn sin(x: double): double as local_sin;
extern fn cos(x: double): double as local_cos;
extern fn tan(x: double): double as local_tan;
extern fn log(x: double): double as local_log;

pub struct Math {
    pub fn sqrt(x: double): double {
        return local_sqrt(x);
    }

    pub fn pow(base: double, exp: double): double {
        return local_pow(base, exp);
    }

    pub fn sin(x: double): double {
        return local_sin(x);
    }

    pub fn cos(x: double): double {
        return local_cos(x);
    }

    pub fn tan(x: double): double {
        return local_tan(x);
    }

    pub fn log(x: double): double {
        return local_log(x);
    }
}
