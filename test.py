import os
import subprocess
import filecmp

good_tests_dir = "./tests/good/"
bad_tests_dir = "./tests/bad/"
main_executable = "./latc_llvm"

GREEN = '\033[92m'
RED = '\033[91m'
ENDC = '\033[0m'

expected_output = {
    "bad001.lat": "\"syntax error at line 1, column 1 before `/'\"",
    "bad002.lat": "\"syntax error at end of file\"",
    "bad003.lat": "int main() function not defined.",
    "bad004.lat": "\"syntax error at line 1, column 9 before `)'\"",
    "bad005.lat": "\"syntax error at line 1, column 4 before `('\"",
    "bad006.lat": "Undeclared variable: x, at line 2, column 9.",
    "bad007.lat": "Redeclaration of: x, at line 3, column 13.",
    "bad008.lat": "`return` keyword did not occur in each possible branch of a function: main, at line 1, column 3.",
    "bad009.lat": "Mismatching types: expected Int got Bool, at line 3, column 9.",
    "bad010.lat": "Mismatching types: expected Int got Void, at line 3, column 17.",
    "bad011.lat": "Mismatching types: expected Int got Bool, at line 2, column 6.",
    "bad012.lat": "`return` keyword did not occur in each possible branch of a function: foo, at line 6, column 1.",
    "bad013.lat": "Mismatching types: expected String got Int, at line 3, column 11.",
    "bad015.lat": "Mismatching types: expected Int got String, at line 4, column 9.",
    "bad016.lat": "Mismatching types: expected String got Int, at line 4, column 9.",
    "bad017.lat": "Wrong number of arguments passed to function: foo, at line 4, column 17.",
    "bad018.lat": "Wrong number of arguments passed to function: foo, at line 4, column 17.",
    "bad019.lat": "Wrong number of arguments passed to function: foo, at line 4, column 17.",
    "bad020.lat": "Mismatching types: expected String got Bool, at line 4, column 7.",
    "bad021.lat": "`return` keyword did not occur in each possible branch of a function: main, at line 5, column 1.",
    "bad022.lat": "Mismatching types: expected Int got String, at line 4, column 6.",
    "bad023.lat": "Mismatching types: expected String got Int, at line 4, column 9.",
    "bad024.lat": "`return` keyword did not occur in each possible branch of a function: main, at line 1, column 1.",
    "bad025.lat": "`return` keyword did not occur in each possible branch of a function: f, at line 5, column 1.",
    "bad026.lat": "Mismatching types: expected Int got String, at line 5, column 3.",
    "bad027.lat": "Mismatching types: expected String got Int, at line 5, column 2.",
    "bad028.lat": "Redeclaration of: x, at line 6, column 1.",
    "bad029.lat": "Not a function: x, at line 3, column 13.",
    "bad030.lat": "Unsupported comparision between instances of type: Int(), at line 10, column 14.",
    "bad031.lat": "Unsupported comparision between instances of type: Array [Int], at line 4, column 14.",
    "bad032.lat": "Unsupported addition between instances of type: Bool, at line 4, column 17.",
    "bad033.lat": "Cannot assign to non-lvalue, at line 2, column 5.",
    "bad034.lat": "Cannot assign to non-lvalue, at line 3, column 5.",
    "bad035.lat": "Undeclared class: List, at line 2, column 10.",
    "bad036.lat": "Undeclared class: List, at line 1, column 1.",
    "bad037.lat": "Mismatching types: expected Class, got: Int, at line 3, column 14.",
    "bad038.lat": "Class: C does not have a field: d, at line 10, column 5.",
    "bad039.lat": "Mismatching types: expected String got Int, at line 9, column 5.",
    "bad040.lat": "Mismatching types: expected Int got Array [Int], at line 9, column 5.",
    "bad041.lat": "Mismatching types: expected Array, got: String, at line 3, column 16.",
    "bad042.lat": "Array index should be an Int, got: Bool, at line 2, column 15.",
    "bad043.lat": "Array index should be an Int, got: Array [Int], at line 3, column 15.",
    "decl_in_if_cond.lat": "\"syntax error at line 3, column 8 before `int'\"",
    "fun_with_void_arg.lat": "Cannot declare a variable which is not a function as `void`, at line 8, column 1.",
    "infinite_while.lat": "`return` keyword did not occur in each possible branch of a function: main, at line 2, column 1.",
    "main_with_arg.lat": "int main() function not defined.",
    "main_with_void_type.lat": "int main() function not defined.",
    "negation.lat": "Mismatching types: expected Int got String, at line 4, column 5.",
    "no_main.lat": "int main() function not defined.",
    "redefined_functions.lat": "Redeclaration of: foo, at line 12, column 1.",
    "redefinition_of_printInt.lat": "Redeclaration of: printInt, at line 8, column 1.",
    "return_void_result.lat": "Cannot return a value from `void` function, at line 9, column 5.",
    "string_decrementation.lat": "Mismatching types: expected Int got String, at line 4, column 5.",
    "string_incrementation.lat": "Mismatching types: expected Int got String, at line 4, column 5.",
    "string_sub.lat": "Mismatching types: expected Int got String, at line 4, column 7.",
    "undeclared_var_as_instr.lat": "Undeclared variable: i, at line 3, column 5.",
    "variable_with_void_type.lat": "Cannot declare a variable which is not a function as `void`, at line 5, column 4.",
    "very_big_int.lat": "Integer value is out of bounds: 1234567891011, at line 4, column 9.",
    "very_long_string.lat": "Length of constant string is out of bounds: 300, at line 5, column 16.",
    "while_true_bad_return_type.lat": "Mismatching types: expected Int got String, at line 7, column 13.",
}


def run_test(test_directory, good_test):
    good = 0
    bad = 0
    filenames = sorted(os.listdir(test_directory))
    
    for filename in filenames:
        if not filename.endswith(".lat"):
            continue

        file_path = os.path.join(test_directory, filename)
        file_prefix = os.path.splitext(file_path)[0]
        bitcode_file = f"{file_prefix}.bc"
        out_file = f"{file_prefix}.output"
        my_out_file = f"{file_prefix}.lli_output"

        process = subprocess.Popen([main_executable, file_path], stderr=subprocess.PIPE)
        std_output, err_output = process.communicate()
        err_output = err_output.decode().split("\n")
        assert std_output is None

        is_good = False
        if good_test:
            assert process.returncode == 0
            assert len(err_output) == 2
            assert err_output[1] == ""
            assert err_output[0] == "OK"

            with open(my_out_file, "w") as my_out:
                process = subprocess.Popen(["lli", bitcode_file], stdout=my_out, text=True)
                process.wait()
            is_good = filecmp.cmp(out_file, my_out_file)
        else:
            assert process.returncode == 1
            assert len(err_output) == 3
            assert err_output[2] == ""
            assert filename in expected_output
            if err_output[0] == "ERROR" and err_output[1] == expected_output[filename]:
                is_good = True

        if is_good:
            good += 1
            print(f"{filename.ljust(35)} {GREEN}Test passed{ENDC}")
        else:
            bad += 1
            print(f"{filename.ljust(35)} {RED}Test failed{ENDC}")
    return (good, good + bad)

# Run good tests
print("Running good tests:")
(x1, y1) = run_test(good_tests_dir, True)

# Run bad tests
print("\nRunning bad tests:")
(x2, y2) = run_test(bad_tests_dir, False)

print(f"{x1 + x2} / {y1 + y2} tests passed.")
