#!/bin/bash
count=$1
if [ $1x = x ] ; then
count=1
fi
# echo ${count}
i=1
cd ../nn-tests
while [ $i -le $count ]
do
	run-nn-tests.sh -json
	# echo ${i}
	# r=`curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'`
	# echo $r
	# curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
	# curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o2'
	# curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o2'
	i=$(( $i + 1 ))
done

