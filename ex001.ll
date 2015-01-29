
@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1


define i32 @add1(i32 %a, i32 %b) {
entry:
  %tmp1 = add i32 %a, %b
  ret i32 %tmp1
}


define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  %z = alloca i32, align 4
  store i32 %argc, i32* %z, align 4
  %4 = load i32* %z, align 4
  %5 = load i32* %z, align 4
  %6 = call i32 @add1(i32 %4, i32 %5)
  store i32 %5, i32* %z, align 4
  %7 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i32 %6)

  ret i32 0
}

declare i32 @printf(i8*, ...) #1
