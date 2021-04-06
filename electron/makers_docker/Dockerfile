FROM debian
RUN dpkg --add-architecture i386
RUN apt update && apt install -y \
   git \
   zip \
   openssh-client \
   nodejs \
   yarnpkg
RUN ln -s /usr/bin/yarnpkg /usr/bin/yarn
#
#  hmmm. this should be done this way in one step,
# it does not work. Wine, then add arch, then wine32.
#
RUN apt update && apt install -y \
  wine \
  mono-complete
RUN dpkg --add-architecture i386
RUN apt update && apt install -y wine32
WORKDIR /build_context
CMD ["yarn", "yarn make:win"]
