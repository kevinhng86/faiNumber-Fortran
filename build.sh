#!/bin/bash

# Copyright 2019 Khang Hoang Nguyen
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Any help with the build file(s) are welcome.
# faiNumber build file for gfortran.
#
# @note  This file supports building the library with individual support
#        to 32/64/128-bit. If 128-bit support is not available. Build a 64
#        and a 32-bit libary file then combine the "mod" directory 
#        together.
#        
#        Take note that currently this script will always delete the
#        output directory if it existed.
compiler="gfortran"
compiler_flag=""
static_or_dynamic=0
build_type=0            # <- 0 - all-in-one
                        #    1 - individual library file
                        #   32 - 32-bit library only
                        #   64 - 64-bit library only
                        #  128 - 128-bit library only
read_temp=""

##################### Custom gfortran command  #########################
echo "Warning: Any command can be set, but only gfortran is supported(7,8)"
echo "Earlier version may or may not work."
echo "Set the compiler command or empty for 'gfortran':"
read -r read_temp
if [ ! -z "$read_temp" ]; then
    compiler=$read_temp
fi
echo ""
################### End custom gfortran command  #######################

################### Verify if gfortran is installed ####################
if ! compiler_string=$("$compiler" --version); then
    echo "Error: ${compiler} is not installed."
    exit 1
fi
################# End verify if gfortran is installed ##################

################################# DIRS #################################
# Everything in dir_output and dir_temp will be delete if existed
dir_src="./src"
dir_output="./lib"
dir_mod="${dir_output}/mod"
dir_temp="./buildtemp"
dir_test="./test"
############################### End DIRS ###############################

############################### Functions ##############################
function exit_with_code() {
    local code=$1
    local clean=$2
    if [ "$clean" -eq 1 ]; then
        rm -rf "$dir_temp"
        rm -rf "$dir_output"
    elif [ "$clean" -eq 2 ]; then
        rm -rf "$dir_temp"
    fi
    exit "$code"
}

# @note  The compile_single() function should only be use after the 
#        compile_o_files and compile_mod_files variable have been
#        declared.
function compile_single(){
    local file=$1
    local compiler=$2
    local compiler_flag=$3
    local file_name="${file%.*}"
    local file_o="${dir_temp}/${file_name}.o"
    local file_mod="${file_name,,}.mod"
 
    if ! eval "${compiler} ${compiler_flag} -J ${dir_temp}/ -c ${dir_src}/${file} -o ${file_o}"; then
        exit_with_code 1 1
    fi
    
    compiled_o_files+=("$file_o")
    compiled_mod_files+=("$file_mod")
}
############################# End functions ############################

############## Verify if all the file in the repo existed ##############
# This will be moved to file.list when faiNumber-Fortran expand to be 
# more than 30 files.
#
# @note  Don't change file_consts order. Its order is used for the build
# procedure.
file_consts=("fniConsts.f90" "fniConsts64.f90" "fniConsts128.f90")
file_32=("fniBinaryUtil.f90" "fniDecimalUtil.f90" "fniHexUtil.f90" \
         "fniOctalUtil.f90" "fniInt32Util.f90" "fniNumberStringUtil.f90")
file_64=("fniBinaryUtil64.f90" "fniDecimalUtil64.f90" "fniHexUtil64.f90" \
         "fniOctalUtil64.f90" "fniInt64Util.f90")
file_128=("fniBinaryUtil128.f90" "fniDecimalUtil128.f90" "fniHexUtil128.f90" \
          "fniOctalUtil128.f90" "fniInt128Util.f90")
file_count=19
file_missing=()
file_test_32=("fniBinaryUtilTest.f90" "fniDecimalUtilTest.f90" \
              "fniHexUtilTest.f90" "fniInt32UtilTest.f90" "fniOctalUtilTest.f90" \
              "fniNumberStringUtilTest.f90")
file_test_64=("fniBinaryUtil64Test.f90" "fniDecimalUtil64Test.f90" \
              "fniHexUtil64Test.f90" "fniInt64UtilTest.f90" "fniOctalUtil64Test.f90")
file_test_128=("fniBinaryUtil128Test.f90" "fniDecimalUtil128Test.f90" \
               "fniHexUtil128Test.f90" "fniInt128UtilTest.f90" "fniOctalUtil128Test.f90")
file_test_count=16
file_test_missing=()
compiled_o_files=()
compiled_mod_files=()
if [ $((${#file_consts[@]} + ${#file_32[@]} + ${#file_64[@]} + ${#file_128[@]})) \
     -ne $file_count ]; then
    echo "Error: Incorrect build script setup."
    echo $((${#file_consts[@]} + ${#file_32[@]} + ${#file_64[@]} + ${#file_128[@]}))
    exit 1
fi
if [ $((${#file_test_32[@]} + ${#file_test_64[@]} + ${#file_test_128[@]})) \
     -ne $file_test_count ]; then
    echo "Warning: Incorrect build script setup. Test file count mismatch."
    echo "It may not be possible to executed all test."
    echo $((${#file_test_32[@]} + ${#file_test_64[@]} + ${#file_test_128[@]}))
    echo ""
fi

for file in "${file_consts[@]}" "${file_32[@]}" "${file_64[@]}" "${file_128[@]}"
do
    if [ ! -f "${dir_src}/${file}" ]; then
        file_missing+=("${dir_src}/${file}")
    fi
done
if [ "${#file_missing[@]}" -gt 0 ]; then
    echo "${#file_missing[@]}"
    echo "Error: Missing file in the repo. The following file(s) is/are missing: "
    for file in "${file_missing[@]}"
    do
        echo "$file"
    done
    exit 1
fi

for file in "${file_test_32[@]}" "${file_test_64[@]}" "${file_test_128[@]}"
do
    if [ ! -f "${dir_test}/${file}" ]; then
        file_test_missing+=("${dir_test}/${file}")
    fi
done
if [ "${#file_test_missing[@]}" -gt 0 ]; then
    echo "${#file_test_missing[@]}"
    echo "Warning: Missing test file in the repo."
    echo "It may not be possible to executed all test."
    echo "The following test file(s) is/are missing:"
    for file in "${file_test_missing[@]}"
    do
        echo "${file}"
    done
    echo ""
fi
############ End verify if all the file in the repo existed ############

#################### Getting user input for options ####################
fortran_version=""
echo "Fortran version(i.e. legacy, f95, f2003, f2008, f2018) or empty?"
read -r fortran_version
if [ ! -z "$fortran_version" ]; then
    fortran_version="-std=$fortran_version"
fi
echo ""

echo "Static(0) or Dynamic(1) libary, or empty for defaul(0)?"
read -r read_temp
if [ ! -z "$read_temp" ]; then
    if [ "$read_temp" == "0" ]; then
        static_or_dynamic=0
    elif [ "$read_temp" == "1" ]; then
        static_or_dynamic=1
    else
        echo "Error: Unknown option. 0 for static, 1 for dynamic."
        exit 1
    fi
fi
echo ""

echo "Build type(0, 1, 32, 64, 128) or empty for default(0):"
echo "0 - All-in-one file."
echo "1 - Individual libary file for 32/64/128 bits."
echo "32 - Build 32-bit library only."
echo "64 - Build 64-bit library only."
echo "128 - Build 128-bit library only."
read -r read_temp
if [ ! -z "$read_temp" ]; then
    if [ "$read_temp" == "0" ]; then
        build_type=0
    elif [ "$read_temp" == "1" ]; then
        build_type=1
    elif [ "$read_temp" == "32" ]; then
        build_type=32
    elif [ "$read_temp" == "64" ]; then
        build_type=64
    elif [ "$read_temp" == "128" ]; then
        build_type=128
    else
        echo "Error: Unknown option. Only support(0, 1, 32, 64, or 128)."
        exit 1
    fi
fi
echo ""
################## End getting user input for options ##################

########################## Compiling section ###########################
echo "Compiling with: "
echo "$compiler_string"
echo ""

if [ $static_or_dynamic -eq 1 ]; then
    compiler_flag="-fPIC"
fi
if [ ! -z "$fortran_version" ]; then
    compiler_flag="$compiler_flag $fortran_version"
fi 

if [ -d "$dir_output" ]; then
    if ! rm -rf "$dir_output"; then
        echo "Error: output directory '$dir_output' existed, but couldn't be removed"
        exit 1
    fi
fi
if [ -d "$dir_temp" ]; then
    if ! rm -rf "$dir_temp"; then
        echo "Error: temp directory '$dir_temp' existed but couldn't be removed"
        exit 1
    fi
fi

if ! mkdir "$dir_temp"; then
    echo "Error: couldn't make $dir_temp"
    exit 1
fi 
if ! mkdir "$dir_output"; then
    echo "Error: couldn't make $dir_output"
    exit 1
fi 
if ! mkdir "$dir_mod"; then
    echo "Error: couldn't make $dir_mod"
    exit 1
fi 

### Compile the consts file first depended on mode.
compile_single "${file_consts[0]}" "${compiler}" "${compiler_flag}"
if [ $build_type -eq 0 ] || [ $build_type -eq 1 ] || [ $build_type -eq 64 ]; then
    compile_single "${file_consts[1]}" "${compiler}" "${compiler_flag}"
fi
if [ $build_type -eq 0 ] || [ $build_type -eq 1 ] || [ $build_type -eq 128 ]; then
    compile_single "${file_consts[2]}" "${compiler}" "${compiler_flag}"
fi

### Compiling 32 if $build_type is 0, 1, or 32
if [ $build_type -eq 0 ] || [ $build_type -eq 1 ] || [ $build_type -eq 32 ]; then
    for file in "${file_32[@]}"
    do
        file_name="${file%.*}"
        file_o="${dir_temp}/${file_name}.o"
        file_mod="${file_name,,}.mod"
    
        if ! eval "${compiler} ${compiler_flag} -J ${dir_temp}/ -c ${dir_src}/${file} -o ${file_o}"; then
            exit_with_code 1 1
        fi
    
        compiled_o_files+=("$file_o")
        compiled_mod_files+=("$file_mod")
    done
fi
### Compiling 64 if $build_type is  0, 1, or 64
if [ $build_type -eq 0 ] || [ $build_type -eq 1 ] || [ $build_type -eq 64 ]; then
    for file in "${file_64[@]}"
    do
        file_name="${file%.*}"
        file_o="${dir_temp}/${file_name}.o"
        file_mod="${file_name,,}.mod"
    
        if ! eval "${compiler} ${compiler_flag} -J ${dir_temp}/ -c ${dir_src}/${file} -o ${file_o}"; then
            exit_with_code 1 1
        fi
    
        compiled_o_files+=("$file_o")
        compiled_mod_files+=("$file_mod")
    done
fi
### Compiling 128 if $build_type is  0, 1, or 128
if [ $build_type -eq 0 ] || [ $build_type -eq 1 ] || [ $build_type -eq 128 ]; then
    for file in "${file_128[@]}"
    do
        file_name="${file%.*}"
        file_o="${dir_temp}/${file_name}.o"
        file_mod="${file_name,,}.mod"
    
        if ! eval "${compiler} ${compiler_flag} -J ${dir_temp}/ -c ${dir_src}/${file} -o ${file_o}"; then
            exit_with_code 1 1
        fi
    
        compiled_o_files+=("$file_o")
        compiled_mod_files+=("$file_mod")
    done
fi

if [ $build_type -eq 0 ]; then
    if [ $static_or_dynamic -eq 0 ]; then
        eval "ar rcs ${dir_output}/faiNumber-n.a ${compiled_o_files[*]}"
    elif [ $static_or_dynamic -eq 1 ]; then
        eval "${compiler} -shared ${compiled_o_files[*]} -o ${dir_output}/faiNumber-n.so"
    fi
fi

if [ $build_type -ne 0 ]; then
    if [ $build_type -eq 1 ] || [ $build_type -eq 32 ]; then
        compiled_o_files_str="${compiled_o_files[0]}"
        
        for file in "${file_32[@]}"
        do
            compiled_o_files_str="${compiled_o_files_str} ${dir_temp}/${file%.*}.o"
        done
        
        if [ $static_or_dynamic -eq 0 ]; then
            eval "ar rcs ${dir_output}/faiNumber32-n.a " \
            "${compiled_o_files_str}" "${compiled_o_files[0]}"
        elif [ $static_or_dynamic -eq 1 ]; then
            eval "${compiler} -shared ${compiled_o_files_str} " \
            "-o ${dir_output}/faiNumber32-n.so"
        fi
    fi

    if [ $build_type -eq 1 ] || [ $build_type -eq 64 ]; then
        compiled_o_files_str="${compiled_o_files[0]} ${compiled_o_files[1]}"
        
        for file in "${file_64[@]}"
        do
            compiled_o_files_str="${compiled_o_files_str} ${dir_temp}/${file%.*}.o"
        done
        
        if [ $static_or_dynamic -eq 0 ]; then
            eval "ar rcs ${dir_output}/faiNumber64-n.a " \
            "${compiled_o_files_str}"
        elif [ $static_or_dynamic -eq 1 ]; then
            eval "${compiler} -shared ${compiled_o_files_str} " \
            "-o ${dir_output}/faiNumber64-n.so"
        fi
    fi

    if [ $build_type -eq 1 ] || [ $build_type -eq 128 ]; then
        if [ $build_type -eq 1 ]; then
            compiled_o_files_str="${compiled_o_files[0]} ${compiled_o_files[2]}"
        elif [ $build_type -eq 128 ]; then
            compiled_o_files_str="${compiled_o_files[0]} ${compiled_o_files[1]}"
        fi
        
        for file in "${file_128[@]}"
        do
            compiled_o_files_str="${compiled_o_files_str} ${dir_temp}/${file%.*}.o"
        done
        
        if [ $static_or_dynamic -eq 0 ]; then
            eval "ar rcs ${dir_output}/faiNumber128-n.a " \
            "${compiled_o_files_str}"
        elif [ $static_or_dynamic -eq 1 ]; then
            eval "${compiler} -shared ${compiled_o_files_str} " \
            "-o ${dir_output}/faiNumber128-n.so"
        fi
    fi
fi

for file in "${compiled_mod_files[@]}"
do
    if ! mv "${dir_temp}/${file}" "${dir_mod}/${file}"; then
        echo "Error: Couldn't move the mod files."
        exit_with_code 1 1
    fi
done

if ! rm -rf "${dir_temp}"; then
    echo "Error:  Can't remove ${dir_temp} * files."
    exit_with_code 1 0
fi

echo "Run test(y) or exit?: "
read -r going_to_run_test
echo ""
if [ "$going_to_run_test" == "y" ]; then
    echo "#################### Starting All Tests ####################"
    echo "Note: this will take sometime."
    echo ""
    echo ""
    
    include_extension=""
    include_files=""
    if [ $static_or_dynamic -eq 0 ]; then
        include_extension=".a"
    elif [ $static_or_dynamic -eq 1 ]; then
        include_extension=".so"
    fi

    include_files=""
    if [ $build_type -eq 0 ]; then
        include_files="${dir_output}/faiNumber-n${include_extension}"
    elif [ $build_type -eq 1 ]; then
        include_files="${dir_output}/faiNumber32-n${include_extension} "
        include_files+="${dir_output}/faiNumber64-n${include_extension} "
        include_files+="${dir_output}/faiNumber128-n${include_extension}"
    elif [ $build_type -eq 32 ]; then
        include_files="${dir_output}/faiNumber32-n${include_extension} "
    elif [ $build_type -eq 64 ]; then
        include_files="${dir_output}/faiNumber64-n${include_extension} "
    elif [ $build_type -eq 128 ]; then
        include_files="${dir_output}/faiNumber128-n${include_extension} "
    fi
       
    if ! mkdir "$dir_temp"; then
        echo "Error: Can't make dir temp for test"
        exit 1
    fi
    
    if [ $build_type -eq 0 ] || [ $build_type -eq 1 ] || [ $build_type -eq 32 ]; then
        for file in "${file_test_32[@]}"
        do
            file_test="${dir_test}/${file}"
            if [ ! -f "$file_test" ]; then
                echo "Skipping test file '$file_test'. The file is missing."
                continue
            fi
            
            file_o="${dir_temp}/${file%.*}.o"
            file_exe="${dir_temp}/${file%.*}$"
        
            if ! eval "${compiler} -I $dir_mod -c $file_test -J $dir_temp -o ${file_o}"; then
                echo "Can't compile test code"
                exit_with_code 1 2
            fi

            if ! eval "${compiler} -I $dir_mod -o $file_exe $file_o ${include_files}"; then
                echo "Can't make test code"
                exit_with_code 1 2
            fi
            
            echo "################# Executing file ${file_exe} ###########"
            if ! "$file_exe"; then
                echo "Couldn't run test build file or the test failed."
                exit_with_code 1 2
            fi
        done
    fi

    if [ $build_type -eq 0 ] || [ $build_type -eq 1 ] || [ $build_type -eq 64 ]; then
        for file in "${file_test_64[@]}"
        do
            file_test="${dir_test}/${file}"
            if [ ! -f "$file_test" ]; then
                echo "Skipping test file '$file_test'. The file is missing."
                continue
            fi

            file_o="${dir_temp}/${file%.*}.o"
            file_exe="${dir_temp}/${file%.*}$"
        
            if ! eval "${compiler} -I $dir_mod -c $file_test -J $dir_temp -o ${file_o}"; then
                echo "Can't compile test code"
                exit_with_code 1 2
            fi

            if ! eval "${compiler} -I $dir_mod -o $file_exe $file_o ${include_files}"; then
                echo "Can't make test code"
                exit_with_code 1 2
            fi

            echo "################# Executing file ${file_exe} ###########"
            if ! "$file_exe"; then
                echo "Couldn't run test build file or the test failed."
                exit_with_code 1 2
            fi
        done
    fi

    if [ $build_type -eq 0 ] || [ $build_type -eq 1 ] || [ $build_type -eq 128 ]; then
        for file in "${file_test_128[@]}"
        do
            file_test="${dir_test}/${file}"
            if [ ! -f "$file_test" ]; then
                echo "Skipping test file '$file_test'. The file is missing."
                continue
            fi

            file_o="${dir_temp}/${file%.*}.o"
            file_exe="${dir_temp}/${file%.*}$"
        
            if ! eval "${compiler} -I $dir_mod -c $file_test -J $dir_temp -o ${file_o}"; then
                echo "Can't compile test code"
                exit_with_code 1 2
            fi

            if ! eval "${compiler} -I $dir_mod -o $file_exe $file_o ${include_files}"; then
                echo "Can't make test code"
                exit_with_code 1 2
            fi

            echo "################# Executing file ${file_exe} ###########"
            if ! "$file_exe"; then
                echo "Couldn't run test build file or the test failed."
                exit_with_code 1 2
            fi
        done
    fi

    echo "##############################################################"
    echo "Tests passed for the all the test file that were present."
    echo "##############################################################"
fi

if ! rm -rf "${dir_temp}"; then
    echo "Error: Can't remove ${dir_temp} for some reason"
    exit 1
fi
exit 0
