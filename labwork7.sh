#!/bin/bash
#0 - IN; 1 - OUT; 2 - ERR

error()
{
	if [ $# -gt 0 ]
	then
		echo $1 1>&2 #вывод и ошибки в 1 аргумент
	fi
	exit 1
}

number?()
{
	if ! [ $# -eq 1 ]
	then
		error "number? принимает 1 числовой аргумент. Ни больше - ни меньше."
	else
		if [ $1 -eq $1 ] 2> /dev/null
		then
			return 0
		else
			return 1
		fi
	fi
}

rec() {
  if [ -d "$1" ]; then
    ls "$1" | while read name; do
      rec "$1/$name"
    done
  else
    echo File "$1"
  fi
}

args()
{
	while [ $# -gt 0 ]
	do
		case $1 in
			-t) 
				shift 
				if [ $# -eq 0 ]
				then
					error "Нет времени задержки."
				fi
				TIMEE=$1
				if ! number? $TIMEE
				then
					error "Время задержки - число. Не иначе."
				fi
				shift;;

			-p)
				shift
				if [ $# -eq 0 ]
				then
					error "Нет пути исполняемого скрипта."
				fi
				PATHY=$1
				if ! [ -f $PATHY ]
				then
					error "Исполняемый скрипт - файл. Не иначе."
				fi
				shift;;

			*)
				error "Ну и как этот аргумент воспринимать?";;
		esac
	done

	if [ -z $TIMEE ]
	then
		error "Ну как бы времемени-то нет."
	fi

	if [ -z $PATHY ]
	then
		error "Ну как бы пути-то нет."
	fi
}

main()
{
	PID=-1
	START=`date +%s`
	LOGS_FILE="output_$START.log"
	ERRORS_FILE="errors_$START.log"

	while [ 0 -eq 0 ]; do
		if ! ps -p $PID > /dev/null 2>&1; then
			bash $PATHY 1>>$LOGS_FILE 2>>$ERRORS_FILE & 
			PID=$!
			echo "Pid: $PID"
		else
			echo "Процесс не завершен, повторный запуск не произведен"
		fi
		sleep $TIMEE
	done
}

args $@
main



