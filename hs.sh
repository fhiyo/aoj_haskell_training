#!/bin/bash
# Author: fhiyo

set -eu

usage() {
  echo "Usage: $0 OPTIONS [PROBLEM NUMBER]

  OPTIONS:
  -c, --clean                       Remove files made when build
  -e, --edit     [PROBLEM NUMBER]   Edit source file
  -g, --git-add  [PROBLEM NUMBER]   Staging [PROBLEM NUMBER] to git
  -h, --help                        Print usage
  -l, --lint     [PROBLEM NUMBER]   Check haskell coding style (hlint using)
  -m, --make-env [PROBLEM NUMBER]   Create need directory and file
  -o, --copy     [PROBLEM NUMBER]   Copy problem code
  -r, --run      [PROBLEM NUMBER]   Run haskell program (no input files)
  -t, --test     [PROBLEM NUMBER]   Test the program is green or red

  --add-input    [PROBLEM NUMBER]   Add input text file
  --add-output   [PROBLEM NUMBER]   Add output text file
  "
}

isExist() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <file or dir path>" 1>&2
    exit 1
  fi
  local path_=$1
  if [ ! -e ${path_} ]; then
    echo "${path_}: No such file or directory" 1>&2
    exit 1
  fi
}

generate-new-test-number() {
  if [ $# != 2 ]; then
    echo "Usage: $0 <problem_number> <input/output>" 1>&2
    exit 1
  fi

  readonly local PROBLEM_=$1
  readonly local DIR="test/${PROBLEM_}/$2"

  isExist ${DIR}

  if [[ -z `ls ${DIR}` ]]; then
    new_test_num=1
  else
    new_test_num=`ls test/${PROBLEM_}/input | sort -n | tail -1 | xargs -I{} basename {} .txt`
    ((new_test_num++))
  fi
}

add-input() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  readonly local PROBLEM=$1
  readonly local INPUT="test/${PROBLEM}/input"

  isExist ${INPUT}
  generate-new-test-number ${PROBLEM} "input"
  vim ${INPUT}/${new_test_num}.txt
}

add-output() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  readonly local PROBLEM=$1
  readonly local OUTPUT="test/${PROBLEM}/output"

  isExist ${OUTPUT}
  generate-new-test-number ${PROBLEM} "output"
  vim ${OUTPUT}/${new_test_num}.txt
}

edit() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  readonly local PROBLEM=$1
  readonly local SOURCE="src/${PROBLEM}/${PROBLEM}.hs"

  isExist ${SOURCE}
  vim ${SOURCE}
}

run() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  readonly local PROBLEM=$1
  readonly local SOURCE="src/${PROBLEM}/${PROBLEM}.hs"
  ghc ${SOURCE}
  ./${SOURCE/.hs/}
}

add_to_git() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  readonly local PROBLEM=$1
  readonly local SOURCE="src/${PROBLEM}/${PROBLEM}.hs"
  readonly local INPUT="test/${PROBLEM}/input"
  readonly local OUTPUT="test/${PROBLEM}/output"

  isExist ${SOURCE}
  isExist ${INPUT}
  isExist ${OUTPUT}

  git add ${SOURCE}

  if [[ -z `ls ${INPUT}` ]]; then
    echo -e "\nNo input files.\n"
  else
    git add ${INPUT}
  fi
  if [[ -z `ls ${OUTPUT}` ]]; then
    echo -e "\nNo input files.\n"
  else
    git add ${OUTPUT}
  fi
}

test_() {
  set +e # Do not exit if exit code is not 0.
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  readonly local PROBLEM=$1

  readonly local SOURCE="src/${PROBLEM}/${PROBLEM}.hs"
  readonly local INPUT="test/${PROBLEM}/input"
  readonly local OUTPUT="test/${PROBLEM}/output"

  isExist ${SOURCE}
  isExist ${INPUT}
  isExist ${OUTPUT}

  ghc ${SOURCE}
  for test_case in `ls ${INPUT}`; do
    diff <(cat ${INPUT}/${test_case} | ./${SOURCE/.hs/}) <(cat ${OUTPUT}/${test_case})
    if [ $? != 0 ]; then
      echo -e "test case: ${test_case}  --  Condition RED...\n" 1>&2
    else
      echo -e "test case: ${test_case}  --  Condition GREEN.\n"
    fi
  done

  if [[ -z `ls ${INPUT}` ]]; then
    echo -e "\nNo test files.\n"
  else
    echo -e "\nTest end up.\n"
  fi
  set -e
}

makeEnv() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  readonly local PROBLEM=$1
  readonly local SOURCE="src/${PROBLEM}/${PROBLEM}.hs"
  readonly local INPUT="test/${PROBLEM}/input"
  readonly local OUTPUT="test/${PROBLEM}/output"

  mkdir -p src/${PROBLEM}
  touch ${SOURCE}
  mkdir -p ${INPUT} ${OUTPUT}
}

lint() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  readonly local PROBLEM=$1
  readonly local SOURCE="src/${PROBLEM}/${PROBLEM}.hs"

  isExist ${SOURCE}
  hlint ${SOURCE}
}

copy() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  readonly local PROBLEM=$1
  readonly local SOURCE="src/${PROBLEM}/${PROBLEM}.hs"

  isExist ${SOURCE}
  cat ${SOURCE} | pbcopy
}

clean() {
  set +e # Do not exit if exit code is not 0.
  source_dirs=`find ./src -mindepth 1 -maxdepth 1 -type d`
  for program_dir in ${source_dirs}; do
    program=`basename ${program_dir}`
    pushd ${program_dir} >/dev/null
    rm ${program} ${program}.hi ${program}.o 2>/dev/null
    popd > /dev/null
  done
  set -e
}

if [ $# == 0 ]; then
  usage
  exit 1
fi

# analyse optional arguments
for opt in "$@"; do
  case "${opt}" in
    '-c' | '--clean' )
      clean
      ;;

    '-e' | '--edit' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      edit ${prob_number}
      ;;

    '-g' | '--git-add' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      add_to_git ${prob_number}
      ;;

    '-h' | '--help' )
      usage
      exit 0
      ;;

    '-l' | '--lint' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      lint ${prob_number}
      ;;

    '-m' | '--make-env' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      makeEnv ${prob_number}
      ;;

     '-o' | '--copy' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      copy ${prob_number}
      ;;

    '-r' | '--run' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      run ${prob_number}
      ;;

    '-t' | '--test' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      test_ ${prob_number}
      ;;

    '--add-input' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      add-input ${prob_number}
      ;;

     '--add-output' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      add-output ${prob_number}
      ;;
  esac
done
