#/bin/sh

SLURPDIR=$HOME/.slurp
PIDFILE="$SLURPDIR/slurp.pid"

COMMAND=$1

export sqliteDbPath="$(pwd)/slurp.sqlite3"

stopAll () {
    xargs kill < $PIDFILE
    rm $PIDFILE
}

startAll() {
    mkdir $SLURPDIR
    make -C frontend &
    make -C api &
    echo "$! " >> $PIDFILE
    make -C notifier &
    echo "$! " >> $PIDFILE
    make -C collector &
    echo "$! " >> $PIDFILE
    make -C influxpusher &
    echo "$! " >> $PIDFILE
}

if [ "$COMMAND" = "start" ]; then
    if [ -f $PIDFILE ]; then
        echo "Server already running, killing before starting..."
        stopAll
    fi

    stack build
    startAll
elif [ "$COMMAND" = "stop" ]; then
    if [ ! -f $PIDFILE ]; then
        echo "Server not running, not doing anything."
    else
        stopAll
    fi
else
    echo "Unknown command. Valid commands: start, stop"
fi
