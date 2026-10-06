package main

import (
	"bufio"
	"encoding/json"
	"errors"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"syscall"
	"testing"
	"time"

	ouroboros "github.com/blinklabs-io/gouroboros"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
)

// helperProcessEnv makes the test binary run the helper's own main, so the
// signal disposition of each operation mode is observed on a real process.
const helperProcessEnv = "MIDGARD_NATIVE_CHAIN_SYNC_HELPER_PROCESS"

func TestMain(m *testing.M) {
	if os.Getenv(helperProcessEnv) == "1" {
		main()
		os.Exit(0)
	}
	os.Exit(m.Run())
}

type helperProcess struct {
	cmd    *exec.Cmd
	lines  chan string
	exited chan *os.ProcessState
}

// startHelperProcess runs main with one startup line and no arguments, the
// way the watcher spawns a stream or reward-account helper.
func startHelperProcess(t *testing.T, startup string) *helperProcess {
	t.Helper()
	cmd := exec.Command(os.Args[0])
	cmd.Env = append(os.Environ(), helperProcessEnv+"=1")
	input, err := cmd.StdinPipe()
	if err != nil {
		t.Fatal(err)
	}
	output, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	h := &helperProcess{cmd: cmd, lines: make(chan string, 16), exited: make(chan *os.ProcessState, 1)}
	go func() {
		scanner := bufio.NewScanner(output)
		scanner.Buffer(make([]byte, 0, 64*1024), 1<<20)
		for scanner.Scan() {
			h.lines <- scanner.Text()
		}
		close(h.lines)
		_ = cmd.Wait()
		h.exited <- cmd.ProcessState
	}()
	t.Cleanup(func() { _ = cmd.Process.Kill() })
	if _, err := input.Write([]byte(startup + "\n")); err != nil {
		t.Fatal(err)
	}
	return h
}

// terminate sends SIGTERM and returns how the helper ended; a helper that
// outlives the bound has ignored the owner's signal.
func (h *helperProcess) terminate(t *testing.T) syscall.WaitStatus {
	t.Helper()
	if err := h.cmd.Process.Signal(syscall.SIGTERM); err != nil {
		t.Fatal(err)
	}
	select {
	case state := <-h.exited:
		return state.Sys().(syscall.WaitStatus)
	case <-time.After(5 * time.Second):
		t.Fatal("helper ignored SIGTERM")
		return 0
	}
}

// silentNode accepts one node connection and never answers it, so the helper
// waits in its node handshake until the owner signals it.
func silentNode(t *testing.T) (string, <-chan struct{}) {
	t.Helper()
	socketPath := filepath.Join(t.TempDir(), "node.socket")
	listener, err := net.Listen("unix", socketPath)
	if err != nil {
		t.Fatal(err)
	}
	accepted := make(chan struct{})
	go func() {
		conn, err := listener.Accept()
		if err != nil {
			return
		}
		close(accepted)
		t.Cleanup(func() { _ = conn.Close() })
	}()
	t.Cleanup(func() { _ = listener.Close() })
	return socketPath, accepted
}

func awaitConnection(t *testing.T, accepted <-chan struct{}) {
	t.Helper()
	select {
	case <-accepted:
	case <-time.After(10 * time.Second):
		t.Fatal("helper never connected to the node")
	}
}

func requireSignalDeath(t *testing.T, status syscall.WaitStatus) {
	t.Helper()
	if !status.Signaled() || status.Signal() != syscall.SIGTERM {
		t.Fatalf("helper did not end by SIGTERM: exited=%v status=%d signaled=%v", status.Exited(), status.ExitStatus(), status.Signaled())
	}
}

// A stream helper signalled before it is ready dies by that signal rather
// than reporting an orderly stop it never reached.
func TestStreamHelperSignalledBeforeReadyDiesBySignal(t *testing.T) {
	config := validStartup(t)
	socketPath, accepted := silentNode(t)
	config.SocketPath = socketPath
	h := startHelperProcess(t, startupLine(t, config))
	awaitConnection(t, accepted)
	requireSignalDeath(t, h.terminate(t))
}

// A reward-account helper has no orderly stop; SIGTERM ends it at once
// instead of leaving it to run out its query deadline.
func TestRewardAccountHelperDiesBySignal(t *testing.T) {
	config := validStartup(t)
	socketPath, accepted := silentNode(t)
	config.SocketPath = socketPath
	config.Intersection = wirePoint{Kind: "origin"}
	config.Operation = wireOperation{Kind: "reward_account", Credential: &wireStakeCredential{Type: "Key", Hash: repeat("ab", 28)}, TimeoutMs: 60000}
	h := startHelperProcess(t, startupLine(t, config))
	awaitConnection(t, accepted)
	requireSignalDeath(t, h.terminate(t))
}

// After ready, SIGTERM is the owner's orderly stop: exit status 0 and no
// chain-sync line after ready.
func TestStreamHelperSignalledAfterReadyStopsOrderly(t *testing.T) {
	config := validStartup(t)
	socketPath := filepath.Join(t.TempDir(), "node.socket")
	listener, err := net.Listen("unix", socketPath)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = listener.Close() })
	config.SocketPath = socketPath
	parent, err := pointFromStartup(config.Intersection)
	if err != nil {
		t.Fatal(err)
	}
	nodeTip := chainsync.Tip{Point: pcommon.NewPoint(5000, parent.Hash), BlockNumber: 5000}
	released := make(chan struct{})
	t.Cleanup(func() { close(released) })
	go func() {
		conn, err := listener.Accept()
		if err != nil {
			return
		}
		server, err := ouroboros.New(ouroboros.WithConnection(conn), ouroboros.WithServer(true), ouroboros.WithNodeToNode(false), ouroboros.WithNetworkMagic(1), ouroboros.WithErrorChan(make(chan error, 8)), ouroboros.WithChainSyncConfig(chainsync.Config{
			FindIntersectFunc: func(_ chainsync.CallbackContext, points []pcommon.Point) (pcommon.Point, chainsync.Tip, error) {
				if len(points) == 0 {
					return pcommon.Point{}, nodeTip, chainsync.ErrIntersectNotFound
				}
				return parent, nodeTip, nil
			},
			RequestNextFunc: func(chainsync.CallbackContext) error {
				<-released
				return errors.New("test released")
			},
		}))
		if err != nil {
			return
		}
		defer server.Close()
		<-released
	}()
	h := startHelperProcess(t, startupLine(t, config))
	select {
	case line, open := <-h.lines:
		var ready readyEvent
		if !open || json.Unmarshal([]byte(line), &ready) != nil || ready.Kind != "ready" {
			t.Fatalf("first helper line is not ready: %q", line)
		}
	case <-time.After(10 * time.Second):
		t.Fatal("helper never became ready")
	}
	status := h.terminate(t)
	if !status.Exited() || status.ExitStatus() != 0 {
		t.Fatalf("post-ready stop: exited=%v status=%d signaled=%v", status.Exited(), status.ExitStatus(), status.Signaled())
	}
	if line, open := <-h.lines; open {
		t.Fatalf("line after the orderly stop: %q", line)
	}
}
