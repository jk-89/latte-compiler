; ModuleID = 'runtime.c'
source_filename = "runtime.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, %struct._IO_codecvt*, %struct._IO_wide_data*, %struct._IO_FILE*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type opaque
%struct._IO_codecvt = type opaque
%struct._IO_wide_data = type opaque

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@stdout = external global %struct._IO_FILE*, align 8
@.str.1 = private unnamed_addr constant [4 x i8] c"%d \00", align 1
@.str.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@stdin = external global %struct._IO_FILE*, align 8
@.str.3 = private unnamed_addr constant [15 x i8] c"Malloc error.\0A\00", align 1
@.str.4 = private unnamed_addr constant [15 x i8] c"runtime error\0A\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @printInt(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i64 0, i64 0), i32 noundef %3)
  %5 = load %struct._IO_FILE*, %struct._IO_FILE** @stdout, align 8
  %6 = call i32 @fflush(%struct._IO_FILE* noundef %5)
  ret void
}

declare i32 @printf(i8* noundef, ...) #1

declare i32 @fflush(%struct._IO_FILE* noundef) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @readInt() #0 {
  %1 = alloca i32, align 4
  %2 = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i64 0, i64 0), i32* noundef %1)
  %3 = load i32, i32* %1, align 4
  ret i32 %3
}

declare i32 @__isoc99_scanf(i8* noundef, ...) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @printString(i8* noundef %0) #0 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = load i8*, i8** %2, align 8
  %4 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.2, i64 0, i64 0), i8* noundef %3)
  %5 = load %struct._IO_FILE*, %struct._IO_FILE** @stdout, align 8
  %6 = call i32 @fflush(%struct._IO_FILE* noundef %5)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i8* @readString() #0 {
  %1 = alloca i8*, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i8* null, i8** %1, align 8
  store i64 0, i64* %2, align 8
  %4 = load %struct._IO_FILE*, %struct._IO_FILE** @stdin, align 8
  %5 = call i64 @getline(i8** noundef %1, i64* noundef %2, %struct._IO_FILE* noundef %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = icmp eq i64 %6, -1
  br i1 %7, label %8, label %17

8:                                                ; preds = %0
  %9 = call noalias i8* @malloc(i64 noundef 1) #5
  store i8* %9, i8** %1, align 8
  %10 = load i8*, i8** %1, align 8
  %11 = icmp ne i8* %10, null
  br i1 %11, label %14, label %12

12:                                               ; preds = %8
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.3, i64 0, i64 0))
  call void @exit(i32 noundef 1) #6
  unreachable

14:                                               ; preds = %8
  %15 = load i8*, i8** %1, align 8
  %16 = getelementptr inbounds i8, i8* %15, i64 0
  store i8 0, i8* %16, align 1
  br label %17

17:                                               ; preds = %14, %0
  %18 = load i64, i64* %3, align 8
  %19 = icmp sgt i64 %18, 0
  br i1 %19, label %20, label %34

20:                                               ; preds = %17
  %21 = load i8*, i8** %1, align 8
  %22 = load i64, i64* %3, align 8
  %23 = sub nsw i64 %22, 1
  %24 = getelementptr inbounds i8, i8* %21, i64 %23
  %25 = load i8, i8* %24, align 1
  %26 = sext i8 %25 to i32
  %27 = icmp eq i32 %26, 10
  br i1 %27, label %28, label %33

28:                                               ; preds = %20
  %29 = load i8*, i8** %1, align 8
  %30 = load i64, i64* %3, align 8
  %31 = sub nsw i64 %30, 1
  %32 = getelementptr inbounds i8, i8* %29, i64 %31
  store i8 0, i8* %32, align 1
  br label %33

33:                                               ; preds = %28, %20
  br label %34

34:                                               ; preds = %33, %17
  %35 = load i8*, i8** %1, align 8
  ret i8* %35
}

declare i64 @getline(i8** noundef, i64* noundef, %struct._IO_FILE* noundef) #1

; Function Attrs: nounwind
declare noalias i8* @malloc(i64 noundef) #2

; Function Attrs: noreturn nounwind
declare void @exit(i32 noundef) #3

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @error() #0 {
  %1 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #6
  unreachable
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i8* @_concatenateStrings(i8* noundef %0, i8* noundef %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca i8*, align 8
  store i8* %0, i8** %3, align 8
  store i8* %1, i8** %4, align 8
  %9 = load i8*, i8** %3, align 8
  %10 = call i64 @strlen(i8* noundef %9) #7
  store i64 %10, i64* %5, align 8
  %11 = load i8*, i8** %4, align 8
  %12 = call i64 @strlen(i8* noundef %11) #7
  store i64 %12, i64* %6, align 8
  %13 = load i64, i64* %5, align 8
  %14 = load i64, i64* %6, align 8
  %15 = add i64 %13, %14
  %16 = add i64 %15, 1
  store i64 %16, i64* %7, align 8
  %17 = load i64, i64* %7, align 8
  %18 = mul i64 %17, 1
  %19 = call noalias i8* @malloc(i64 noundef %18) #5
  store i8* %19, i8** %8, align 8
  %20 = load i8*, i8** %8, align 8
  %21 = icmp ne i8* %20, null
  br i1 %21, label %24, label %22

22:                                               ; preds = %2
  %23 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.3, i64 0, i64 0))
  call void @exit(i32 noundef 1) #6
  unreachable

24:                                               ; preds = %2
  %25 = load i8*, i8** %8, align 8
  %26 = getelementptr inbounds i8, i8* %25, i64 0
  store i8 0, i8* %26, align 1
  %27 = load i8*, i8** %8, align 8
  %28 = load i8*, i8** %3, align 8
  %29 = call i8* @strcat(i8* noundef %27, i8* noundef %28) #5
  store i8* %29, i8** %8, align 8
  %30 = load i8*, i8** %8, align 8
  %31 = load i8*, i8** %4, align 8
  %32 = call i8* @strcat(i8* noundef %30, i8* noundef %31) #5
  store i8* %32, i8** %8, align 8
  %33 = load i8*, i8** %8, align 8
  ret i8* %33
}

; Function Attrs: nounwind readonly willreturn
declare i64 @strlen(i8* noundef) #4

; Function Attrs: nounwind
declare i8* @strcat(i8* noundef, i8* noundef) #2

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @_stringsEqual(i8* noundef %0, i8* noundef %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  store i8* %0, i8** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load i8*, i8** %3, align 8
  %6 = load i8*, i8** %4, align 8
  %7 = call i32 @strcmp(i8* noundef %5, i8* noundef %6) #7
  %8 = icmp eq i32 %7, 0
  ret i1 %8
}

; Function Attrs: nounwind readonly willreturn
declare i32 @strcmp(i8* noundef, i8* noundef) #4

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @_stringsNotEqual(i8* noundef %0, i8* noundef %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  store i8* %0, i8** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load i8*, i8** %3, align 8
  %6 = load i8*, i8** %4, align 8
  %7 = call zeroext i1 @_stringsEqual(i8* noundef %5, i8* noundef %6)
  %8 = xor i1 %7, true
  ret i1 %8
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { noreturn nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { nounwind readonly willreturn "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #5 = { nounwind }
attributes #6 = { noreturn nounwind }
attributes #7 = { nounwind readonly willreturn }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"Ubuntu clang version 14.0.0-1ubuntu1.1"}
