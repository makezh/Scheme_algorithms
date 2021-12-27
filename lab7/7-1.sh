#!/bin/bash
#0 - IN; 1 - OUT; 2 - ERR

rec() {
  if [ -d "$1" ]
  then
    ls "$1" | while read name; do
      rec "$1/$name"
    done
  else
    echo File "$1"
  fi
}

error()
{
	if [ $# -gt 0 ]
	then
		echo $1 1>&2 #stdout в stderr 
	fi
	exit 1
}

number?()
{
	if ! [ $# -eq 1 ]
	then
		error "number? принимает 1 числовой аргумент. Ни больше - ни меньше."
	else
		if [ $1 -eq $1 ] 2> /dev/null #выбрасываем stderr
		then
			return 0
		else
			return 1
		fi
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
				TIME_=$1
				if ! number? $TIME_
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
				PATH_=$1
				if ! [ -f $PATH_ ]
				then
					error "Исполняемый скрипт - файл. Не иначе."
				fi
				shift;;

			*)
				error "Ну и как этот аргумент воспринимать?";;
		esac
	done

	if [ -z $TIME_ ]
	then
		error "Ну как бы времемени-то нет."
	fi

	if [ -z $PATH_ ]
	then
		error "Ну как бы пути-то нет."
	fi
}

main()
{
	PID=-1
	START=`date +%s`
	LOGS_FILE="output_$START.log" # формируем ЛОГ-файл для вывода
	ERRORS_FILE="errors_$START.log" # формируем ЛОГ-файл для ошибок

	while [ 0 -eq 0 ]
	do
		if ! ps -p $PID > /dev/null 2>&1 #вывод всех Пидов в небытие. stderr в stdout
		then
			bash $PATH_ 1>>$LOGS_FILE 2>>$ERRORS_FILE & 
			PID=$! #идентификатор процесса последнего задания
			echo "Pid: $PID"
		else
			echo "Почему закончилось?"
		fi
		sleep $TIME_
	done
}

args $@
main



