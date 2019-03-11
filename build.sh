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
# faiNumber build file for gfortran, (Ubuntu 18.04).
compiler="gfortran"
compiler_flag=""
static_or_dynamic=0
build_type=0            # <- 0 - All; 1 - Separate units; 2 - All in one
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
############################# End functions ############################

############## Verify if all the file in the repo existed ##############
# This will be moved to file.list when faiNumber-Fortran expand to be 
# more than 30 files.
#
# @note  Don't change file_consts order. Its order is used for the build
# procedure.
file_consts=("fnConsts.f90" "fnConsts64.f90" "fnConsts128.f90")
file_32=("fnBinaryUtil.f90" "fnDecimalUtil.f90" "fnHexUtil.f90" \
         "fnOctalUtil.f90" "fnInt32Util.f90" "fnNumberStringUtil.f90")
file_64=("fnBinaryUtil64.f90" "fnDecimalUtil64.f90" "fnHexUtil64.f90" \
         "fnOctalUtil64.f90" "fnInt64Util.f90")
file_128=("fnBinaryUtil128.f90" "fnDecimalUtil128.f90" "fnHexUtil128.f90" \
          "fnOctalUtil128.f90" "fnInt128Util.f90")
file_count=19
file_missing=()
file_all=("${file_consts[@]}" "${file_32[@]}" "${file_64[@]}" "${file_128[@]}")
compiled_o_files=()
compiled_mod_files=()
if [ ${#file_all[@]} -ne $file_count ]; then
    echo "${#file_all[@]}"
    exit 1
fi

for file in "${file_all[@]}"
do
    if [ ! -f "$dir_src/$file" ]; then
        file_missing+=("$file")
    fi
done
if [ "${#file_missing[@]}" -gt 0 ]; then
    echo "${#file_missing[@]}"
    echo "Error: Missing file in repo. The following file(s) is/are missing: "
    for file in "${file_missing[@]}"
    do
        echo "$file"
    done
    exit 1
fi
############ End verify if all the file in the repo existed ############

#################### Getting user input for options ####################
fortran_version=""
echo "Fortran version(legacy, f95, f2003, f2018) or empty?"
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

echo "Build type(0, 1) or empty for all-in-one:"
echo "0 - All in one file."
echo "1 - Invidual libary file for 32/64/128 bits."
read -r read_temp
if [ ! -z "$read_temp" ]; then
    if [ "$read_temp" == "0" ]; then
        build_type=0
    elif [ "$read_temp" == "1" ]; then
        build_type=1
    else
        echo "Error: Unknown option. 0 for all-in-one, 1 for individual lib files."
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
    rm -rf "$dir_output" || exit 1
fi
if [ -d "$dir_temp" ]; then
    rm -rf "$dir_temp" || exit 1
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

for file in "${file_all[@]}"
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

if [ $build_type -eq 0 ]; then
    if [ $static_or_dynamic -eq 0 ]; then
        eval "ar rcs ${dir_output}/faiNumber.a ${compiled_o_files[*]}"
    elif [ $static_or_dynamic -eq 1 ]; then
        eval "${compiler} -shared ${compiled_o_files[*]} -o ${dir_output}/faiNumber.so"
    fi
fi

if [ $build_type -eq 1 ]; then
    compiled_o_files_str=""
    for file in "${file_32[@]}"
    do
        compiled_o_files_str="${compiled_o_files_str} ${dir_temp}/${file%.*}.o"
    done
    if [ $static_or_dynamic -eq 0 ]; then
        eval "ar rcs ${dir_output}/faiNumber32.a " \
        "${compiled_o_files_str}" "${compiled_o_files[0]}"
    elif [ $static_or_dynamic -eq 1 ]; then
        eval "${compiler} -shared ${compiled_o_files_str} ${compiled_o_files[0]} " \
        "-o ${dir_output}/faiNumber32.so"
    fi

    compiled_o_files_str=""
    for file in "${file_64[@]}"
    do
        compiled_o_files_str="${compiled_o_files_str} ${dir_temp}/${file%.*}.o"
    done
    if [ $static_or_dynamic -eq 0 ]; then
        eval "ar rcs ${dir_output}/faiNumber64.a " \
        "${compiled_o_files_str} ${compiled_o_files[0]} ${compiled_o_files[1]}"
    elif [ $static_or_dynamic -eq 1 ]; then
        eval "${compiler} -shared ${compiled_o_files_str} ${compiled_o_files[0]} ${compiled_o_files[1]} " \
        "-o ${dir_output}/faiNumber64.so"
    fi    

    compiled_o_files_str=""
    for file in "${file_128[@]}"
    do
        compiled_o_files_str="${compiled_o_files_str} ${dir_temp}/${file%.*}.o"
    done
    if [ $static_or_dynamic -eq 0 ]; then
        eval "ar rcs ${dir_output}/faiNumber128.a " \
        "${compiled_o_files_str} ${compiled_o_files[0]} ${compiled_o_files[2]}"
    elif [ $static_or_dynamic -eq 1 ]; then
        eval "${compiler} -shared ${compiled_o_files_str} ${compiled_o_files[0]} ${compiled_o_files[2]} " \
        "-o ${dir_output}/faiNumber128.so"
    fi    
fi

for file in "${compiled_mod_files[@]}"
do
    if ! mv "${dir_temp}/${file}" "${dir_mod}/${file}"; then
        echo "Error: Couldn't move the mod files."
        exit 1 1
    fi
done

if ! rm -rf "${dir_temp}"; then
    echo "Error:  Can't remove ${dir_temp} * files."
    exit_with_code 1
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
        include_files="${dir_output}/faiNumber${include_extension}"
    elif [ $build_type -eq 1 ]; then
        include_files="${dir_output}/faiNumber32${include_extension} "
        include_files+="${dir_output}/faiNumber64${include_extension} "
        include_files+="${dir_output}/faiNumber128${include_extension}"
    fi
       
    if ! mkdir "$dir_temp"; then
        echo "Error: Can't make dir temp for test"
        exit 1
    fi
    
    for file in ${dir_test}/*.f90
    do
        file_name=$(basename "$file")
        file_o="${dir_temp}/${file_name%.*}.o"
        file_exe="${dir_temp}/${file_name%.*}$"
        
        if ! eval "${compiler} -I $dir_mod -c $file -J $dir_temp -o ${file_o}"; then
            echo "Can't compile test code"
            exit_with_code 1 2
        fi       
        
        if ! eval "${compiler} -I $dir_mod -o $file_exe $file_o ${include_files}"; then
            echo "Can't make test code"
            exit_with_code 1 2
        fi
        
        if ! "$file_exe"; then
            echo "Couldn't run test build file"
            exit_with_code 1 2
        fi
    done
    
    echo "##############################################################"
    echo "All test passed."
    echo "##############################################################"
fi

if ! rm -rf "${dir_temp}"; then
    echo "Error: Can't remove ${dir_temp} for some reason"
    exit 1
fi
exit 0
