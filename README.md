ebroker
=====

erlang 版本的代理（双发机制+连接池）主要解决在有限时耗下，通过双发机制解决连接超时问题，提高成功率。

Env Build

安装依赖文件

1. yum -y install gcc gcc-c++ kernerl-devel
2. yum install unixODBC unixODBC-devel
3. yum install ncurses-devel
4. yum install openssl-devel

编译安装erlang

1. wget http://erlang.org/download/otp_src_20.2.tar.gz
2. tar -xvf otp_src_20.2.tar.gz
3. cd otp_src_20.2
4. ./configure --prefix=/usr/  --with--ssl
5. make && make  install

安装rebar3

1. wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
2. rebar3 local install

编译运行ebroker

1. 编译 rebar3 compile  调试启动 rebar3 shell --apps ebroker

2. rebar3 release -d false

3. rebar3 as prod tar

4. 解压项目，找到可执行文件运行即可，相关配置可自行到配置文件修改
