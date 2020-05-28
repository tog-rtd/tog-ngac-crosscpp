server=141.58.0.8
repo_port=2777
echo "obtaining token ....";
 mytoken=`curl -s -H "Content-Type: text/plain" -XGET http://${server}:${repo_port}/login?email="r.delong@opengroup.org"\&pw="0220057633"`  ;
     echo "token is ${mytoken}";

