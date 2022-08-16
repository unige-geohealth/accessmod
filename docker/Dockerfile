FROM fredmoser/accessmod_base:5.7-b
LABEL maintainer="frederic.moser@unige.ch"
WORKDIR /app
# Copy context is expected to be app root
COPY . .
CMD Rscript --vanilla run.r
