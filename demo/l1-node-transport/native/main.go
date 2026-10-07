// Command midgard-l1-node-transport is the long-lived N2C transport sidecar
// of one Midgard role. It speaks the length-prefixed frame protocol of
// README.md on stdin/stdout and holds the role's node connection. It moves
// bytes only: it makes no qualification, depth, finality or validity
// decision.
package main

import (
	"fmt"
	"os"
	"os/signal"
	"syscall"
)

func main() {
	if len(os.Args) == 2 && os.Args[1] == "--protocol-version" {
		fmt.Println(protocolVersion)
		return
	}
	if len(os.Args) != 1 {
		fmt.Fprintln(os.Stderr, "usage: midgard-l1-node-transport [--protocol-version]")
		os.Exit(exitClientMisuse)
	}
	signals := make(chan os.Signal, 1)
	signal.Notify(signals, syscall.SIGINT, syscall.SIGTERM)
	stop := make(chan struct{})
	go func() {
		<-signals
		close(stop)
	}()
	os.Exit(runSession(os.Stdin, os.Stdout, os.Stderr, stop))
}
