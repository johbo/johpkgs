module daemon_fg;

import core.sys.posix.signal;
import core.thread;
import core.time;
import std.array;
import std.conv;
import std.experimental.logger;
import std.file;
import std.getopt;
import std.path;
import std.process;
import std.stdio;


int watchedPid;


int main(string[] args) {

    string pidFile;
    bool logDebug = false;

    auto opts = getopt(
        args,
        "pid-file", "Path of the PID file to watch.", &pidFile,
        "debug", "Log debugging information", &logDebug);

    if (!pidFile.length) {
        writeln("Must provide --pid-file");
        return 1;
    } else {
        pidFile = pidFile.expandTilde();
    }
    infof("Watching PID file: %s", pidFile);

    if (opts.helpWanted) {
        defaultGetoptPrinter(
            "Stays in foreground as long as daemon is running.",
            opts.options);
        return 0;
    }

    // Initialize logging
    sharedLog.logLevel = logDebug ? LogLevel.all : LogLevel.info;

    // The first parameter is the command name, skipt it. All others are not
    // consumed by the getopt call above and thus should be the subcommand
    // call.
    auto subcommandArgs = args[1..$];
    startSubprocess(subcommandArgs);

    watchedPid = readText(pidFile).to!int;
    connectSignalForwarders();
    waitUntilProcessTerminates(watchedPid);

    return 0;
}


void startSubprocess(string[] args) {
    infof("Starting subprocess %s", args.join(" "));
    args[0] = args[0].expandTilde();
    spawnProcess(args);

    // TODO: Wait up to N seconds and then fail if the pid file and process do
    // not yet match.
    Thread.sleep(dur!"msecs"(500));
}


void connectSignalForwarders() {
    sigset(SIGINT, &forwardSignal);
    sigset(SIGTERM, &forwardSignal);
}


void waitUntilProcessTerminates(int pid) {
    infof("Waiting until %s is not running anymore", pid);
    while(isProcessRunning(pid)) {
        log("process is still alive");
        Thread.sleep(dur!"msecs"(500));
    }
    infof("Process %s terminated, terminating as well...", pid);
}


bool isProcessRunning(int pid) {
    // 0 means that only the error checking is performed, that is used to find
    // out if the process is still running.
    enum NO_SIGNAL = 0;

    auto result = kill(pid, NO_SIGNAL);
    return result == 0;
}

extern(C)
void forwardSignal(int value) {
    infof("Forwarding signal %s to process %s", value, watchedPid);
    auto result = kill(watchedPid, value);
    if(result != 0) {
        warning("Sending the signal failed");
    }
}
