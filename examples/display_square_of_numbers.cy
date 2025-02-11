import io; 

fn display_double_square(square_size: i32) {
    for (#i = 1; i <= square_size; i++) { // col
        for (#j = 1; j <= square_size; j++) { // row
            IO.printf("%d ", j);
        }

        IO.printf(" ");

        for (#j = 1; j <= square_size; j++) { // row
            IO.printf("%d ", j);
        }
        
        IO.printf("\n");
    }
}

pub fn main() {
    #square_size: i32 = 4;

    for (#sizer = 0; sizer < 3; sizer++) {
        display_double_square(square_size);
        IO.printf("\n");
        display_double_square(square_size);
        square_size = square_size + 4;
        
        IO.printf("\n");
        IO.printf("\n");
    }
}