#!/bin/sh
openssl req -config localhost.conf -new -x509 -sha256 -newkey rsa:2048 -nodes -keyout localhost.key.pem -days 365 -out localhost.cert.pem
