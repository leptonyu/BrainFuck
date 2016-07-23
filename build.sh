#!/bin/sh
PRG="$0"
# Need this for relative symlinks.
while [ -h "$PRG" ] ; do
    ls=`ls -ld "$PRG"`
    link=`expr "$ls" : '.*-> \(.*\)$'`
    if expr "$link" : '/.*' > /dev/null; then
        PRG="$link"
    else
        PRG=`dirname "$PRG"`"/$link"
    fi
done

ROOT=`dirname "$PRG"`
ROOT=`cd "$ROOT";pwd`

cd "$ROOT"

if [ ! -f "$ROOT/make.sh" ]; then
  echo "make.sh not found!"
  exit 1
fi

FROM=`head -1 "$ROOT/make.sh" | grep -o 'FROM [0-9a-z][-0-9a-z]*\(:[0-9a-z][-.0-9a-z]*\)\?' | awk '{print $2}'`
if [ -z "$FROM" ]; then
  echo "FROM not set in first line of make.sh"
  exit 1
fi

NAME=`basename "$ROOT"`
LABEL=icymint/$NAME
TEMP_LABEL=$LABEL-builder
FILE="$ROOT/Dockerfile"
PACK=true


if [ "true" = "$PACK" -a ! -f "upx" ]; then
  echo "Downloading upx..."
  curl -sL "https://github.com/leptonyu/http-server-in-docker/raw/master/upx" -o upx
  if [ ! -f upx ]; then
    echo "upx not found !"
    exit 1
  fi
fi

if [ ! -e "stack" ]; then
  echo "Preare stack ..."
  mkdir stack
  docker run --rm -v "$ROOT":/src -w /src -v "$ROOT/stack":/root/.stack $FROM stack install
  if [ $? -ne 0 ]; then
    rm -rf stack
    echo "prepare stack failed"
    exit 1
  fi
fi

echo "FROM $FROM"    > "$FILE"
cat "$ROOT/make.sh" | grep '^#' | grep -o '\(ARG\|ENV\|VOLUME\|EXPOSE\|ADD\|COPY\|MAINTAINER\|USER\|ONBUILD\) ..*' >> "$FILE"
cat >> "$FILE" <<-EOF
### FROM in make.sh

ARG PACK=true

COPY upx /bin/upx
COPY make.sh make.sh

RUN chmod +x make.sh \
 && ./make.sh \
 && eval [ -f main ] \
 && if [ "\$PACK" = "true" ]; then \
         chmod +x /bin/upx \
      && upx --lzma --best main \
  ; fi \
 && echo "FROM scratch"            > Dockerfile \
 && echo "COPY main  /main"       >> Dockerfile \
 && echo "ENTRYPOINT [\"/main\"]" >> Dockerfile

CMD tar -cf - main Dockerfile
EOF

cat "$FILE"

TID=`docker images -q $TEMP_LABEL`
if [ -n "$TID" ]; then
  docker rmi -f $TEMP_LABEL
fi

ID=`docker images -q $LABEL`
if [ -n "$ID" ]; then
  docker rmi -f "$ID"
fi

GO=`docker images -q $FROM`

docker build --rm --no-cache -t $TEMP_LABEL --build-arg PACK=$PACK . \
&& docker run --rm $TEMP_LABEL | docker build --rm --no-cache -t $LABEL -

TID=`docker images -q $TEMP_LABEL`
if [ -n "$TID" ]; then
  docker rmi -f $TEMP_LABEL
fi

rm -f "$FILE"

if [ -z "$GO" ]; then
  docker rmi -f $FROM
fi
