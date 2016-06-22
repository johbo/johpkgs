set -e

source $stdenv/setup

mkdir -p $out/bin

cat > $out/bin/control <<EOF
mkdir -p $logDir
chown -R $user $logDir
export PATH=$PATH:$su/bin

start()
{
  su $user -s /bin/sh -c "$jboss/bin/standalone.sh \
      -Djboss.server.base.dir=$serverDir \
      -Djboss.server.base.url=file://$serverDir \
      -Djboss.server.temp.dir=$tempDir \
      -Djboss.server.log.dir=$logDir \
      -P=$serverDir/configuration/zanata.properties
      -c standalone-zanata.xml"
}

stop()
{
  su $user -s /bin/sh -c "$zanata/bin/shutdown.sh -S"
}

if test "\$1" = start
then
  trap stop 15

  start
elif test "\$1" = stop
then
  stop
elif test "\$1" = init
then
  echo "Are you sure you want to create a new server instance (old server instance will be lost!)?"
  read answer

  if ! test \$answer = "yes"
  then
    exit 1
  fi

  rm -rf $serverDir
  mkdir -p $serverDir
  cd $serverDir
  cp -av $zanata/standalone .

  # Make files accessible for the server user
  chown -R $user $serverDir
  for i in \`find $serverDir -type d\`
  do
    chmod 755 \$i
  done
  for i in \`find $serverDir -type f\`
  do
    chmod 644 \$i
  done
fi
EOF

chmod +x $out/bin/*
