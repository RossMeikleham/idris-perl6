FROM dgellow/idris

# Install Perl6
RUN groupadd -r perl6 && useradd -r -g perl6 perl6

ARG rakudo_version=2018.01
ENV rakudo_version=${rakudo_version}

RUN buildDeps=' \
		curl \
        gcc \
        libc6-dev \
        libencode-perl \
        make \
    ' \
    && set -x \
    && apt-get update \
    && apt-get --yes install --no-install-recommends $buildDeps \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir /root/rakudo \
    && curl -fsSL http://rakudo.org/downloads/star/rakudo-star-${rakudo_version}.tar.gz -o rakudo.tar.gz \
    && tar xzf rakudo.tar.gz --strip-components=1 -C /root/rakudo \
    && ( \
        cd /root/rakudo \
        && perl Configure.pl --prefix=/usr --gen-moar \
        && make install \
    ) 

ENV PATH=$PATH:/usr/share/perl6/site/bin


# Install idris-perl6
ADD . /app
WORKDIR /app

RUN cabal update && cabal configure && cabal install --only-dependencies && cabal install
