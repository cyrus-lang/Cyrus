; ModuleID = 'hello'
source_filename = "hello"

define i64 @sample_func(i64 %0, i64 %1) {
entry:
  %temp.1 = add i64 %0, %1
  ret i64 %temp.1
}