

curl -X POST http://localhost:8000/new -H Content-Type:multipart/form-data --form "name=/x/y/"
# expected: 500 - Internal Server Error
