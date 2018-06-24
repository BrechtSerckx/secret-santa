FROM brechtserckx/heroku-haskell-stack
MAINTAINER Brecht Serckx <brecht_serckx@hotmail.com>

RUN ["chmod", "+x", "/app/user/secret-santa"]

RUN useradd -m secretsanta
USER secretsanta

EXPOSE 3000

CMD /app/user/secret-santa
