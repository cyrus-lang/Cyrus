extern fn sqrt(x: double): double;

struct Math {
    fn sqrt(self: *Math, x: double): double {
        return sqrt(x);
    }
}
