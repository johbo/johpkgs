set -e

source $stdenv/setup

mkdir -p $out/bin

cat > $out/bin/control <<EOF
mkdir -p $logDir
chown -R $user $logDir
export PATH=$PATH:$su/bin

echo $zanata > $logDir/path-zanata
echo $out > $logDir/path-control

start()
{
  su $user -s /bin/sh -c "$zanata/bin/standalone.sh \
      -Djboss.server.base.dir=$serverDir \
      -Djboss.server.temp.dir=$tempDir \
      -Djboss.server.log.dir=$logDir \
      -P=$serverDir/configuration/zanata.properties \
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
fi
EOF

chmod +x $out/bin/*
