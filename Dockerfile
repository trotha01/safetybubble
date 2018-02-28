FROM quay.io/travisci/travis-jvm

RUN sudo su travis
RUN rvm install 2.3.0
RUN rvm use 2.3.0

WORKDIR builds

RUN git clone https://github.com/travis-ci/travis-build.git \
cd travis-build \
gem install travis \
travis # to create ~/.travis \
ln -s `pwd` ~/.travis/travis-build \
bundle install


