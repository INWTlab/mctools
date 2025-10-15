# Author: Michelle Golchert
FROM inwt/r-batch:4.5.1

ADD . .

RUN installPackage
