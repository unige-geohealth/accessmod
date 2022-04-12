FROM --platform=linux/amd64 debian:stable
RUN dpkg --add-architecture i386
ENV PATH="/usr/local/sbin:/usr/local/bin:"$PATH
ENV NODE_PATH="/usr/lib/nodejs:/usr/share/nodejs"
WORKDIR /tmp 
RUN apt update && apt install -y \
   git \
   zip \
   openssh-client \
   curl \
   && curl -L https://raw.githubusercontent.com/tj/n/master/bin/n -o n \
   && bash n lts \
   && npm install -g yarn 
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
