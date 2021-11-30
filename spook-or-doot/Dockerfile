FROM ruby:2.7.1

COPY . /app

WORKDIR /app

RUN bundle

RUN mkdir /app/tmp && chown www-data:www-data /app/tmp

CMD rails s
