package main

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	ouroboros "github.com/blinklabs-io/gouroboros"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
	"io"
	"log/slog"
	"net"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"
)

func writeChainSyncFailure(writer *canonicalWriter, diagnostics io.Writer, cause error) error {
	// stderr precedes the unchanged terminal stdout schema so the owner can
	// retain the concrete cause without admitting it as chain-sync data.
	_, _ = fmt.Fprintf(diagnostics, "native chain-sync failed: %v\n", cause)
	return writer.write(errorEvent{Code: "chain_sync_failed", Kind: "error", SchemaVersion: schemaVersion})
}

func main() {
	writer := &canonicalWriter{encoder: json.NewEncoder(os.Stdout)}
	if len(os.Args) == 2 && os.Args[1] == exactPointServiceFlag {
		signals := make(chan os.Signal, 1)
		signal.Notify(signals, syscall.SIGINT, syscall.SIGTERM)
		service := newExactPointService(os.Stdout)
		go func() {
			<-signals
			service.shutdown()
			os.Exit(0)
		}()
		os.Exit(service.serve(os.Stdin))
	}
	config, startupCanonical, err := readStartup()
	// Exact-point queries are sessions of the persistent service only; one
	// spawned process per query is not an admitted operation mode.
	if err != nil || len(os.Args) != 1 || config.Operation.Kind == "exact_point" {
		_ = writer.write(errorEvent{Code: "invalid_startup", Kind: "error", SchemaVersion: schemaVersion})
		os.Exit(64)
	}
	if config.Operation.Kind == "reward_account" {
		if err := writeRewardAccount(config, startupCanonical, writer); err != nil {
			_ = writer.write(errorEvent{Code: "reward_account_query_failed", Kind: "error", SchemaVersion: schemaVersion})
			fmt.Fprintln(os.Stderr, err)
			os.Exit(69)
		}
		return
	}
	// A stream helper keeps the default signal disposition until it is ready,
	// so an owner signal before ready still ends it by that signal; after
	// ready, a signal is an orderly stop with exit status 0.
	stop := make(chan struct{})
	armStop := func() {
		signals := make(chan os.Signal, 1)
		signal.Notify(signals, syscall.SIGINT, syscall.SIGTERM)
		go func() {
			<-signals
			close(stop)
		}()
	}
	if status := runChainSync(config, startupCanonical, writer, os.Stderr, stop, armStop); status != 0 {
		os.Exit(status)
	}
}

// runChainSync owns one node connection for one admitted startup and returns
// the helper exit status for its outcome; 0 means the owner closed stop. A
// stopped session seals its writer before its connection is interrupted, so
// no chain-sync line follows the owner's close.
func runChainSync(config startupConfig, startupCanonical []byte, writer *canonicalWriter, diagnostics io.Writer, stop <-chan struct{}, onReady func()) int {
	exact := config.Operation.Kind == "exact_point"
	errorChannel := make(chan error, 4)
	readyGate := make(chan struct{})
	var releaseReadyGate sync.Once
	chainSyncConfig := makeChainSyncConfig(config, writer, readyGate)
	dialTimeout := 10 * time.Second
	var queryDeadline time.Time
	if exact {
		queryDeadline = time.Now().Add(time.Duration(config.Operation.TimeoutMs) * time.Millisecond)
		dialTimeout = min(dialTimeout, time.Until(queryDeadline))
	}
	ctx, cancel := context.WithCancel(context.Background())
	var connection *ouroboros.Connection
	defer func() {
		// Seal before releasing callbacks parked on the ready gate, then
		// interrupt every connection-owned read through the socket. Shutdown
		// never waits on the connection, so a session end cannot wedge.
		writer.seal()
		cancel()
		releaseReadyGate.Do(func() { close(readyGate) })
		if connection != nil {
			go connection.Close()
		}
	}()
	go func() {
		select {
		case <-stop:
			writer.seal()
			cancel()
		case <-ctx.Done():
		}
	}()
	stopped := func() bool {
		select {
		case <-stop:
			return true
		default:
			return false
		}
	}
	fail := func(code string, status int) int {
		if stopped() {
			return 0
		}
		_ = writer.write(errorEvent{Code: code, Kind: "error", SchemaVersion: schemaVersion})
		return status
	}
	dialer := net.Dialer{Timeout: dialTimeout}
	socket, err := dialer.DialContext(ctx, "unix", config.SocketPath)
	if err != nil {
		return fail("node_handshake_failed", 69)
	}
	context.AfterFunc(ctx, func() { _ = socket.Close() })
	var transport net.Conn = &streamSegmentConn{Conn: socket}
	if exact {
		if err := socket.SetDeadline(queryDeadline); err != nil {
			return fail("node_deadline_failed", 69)
		}
		transport = &queryLimitedConn{Conn: socket, remaining: maxQueryIngressBytes, deadline: queryDeadline}
	}
	created, err := ouroboros.New(
		ouroboros.WithConnection(transport),
		ouroboros.WithNetworkMagic(config.NetworkMagic),
		ouroboros.WithNodeToNode(false),
		ouroboros.WithErrorChan(errorChannel),
		ouroboros.WithLogger(slog.New(slog.NewJSONHandler(diagnostics, nil))),
		ouroboros.WithChainSyncConfig(chainSyncConfig),
	)
	if err != nil {
		return fail("connection_setup_failed", 70)
	}
	connection = created
	currentTip, err := connection.ChainSync().Client.GetCurrentTip()
	if err != nil {
		return fail("tip_query_failed", 69)
	}
	point, err := pointFromStartup(config.Intersection)
	if err != nil {
		return fail("invalid_intersection", 64)
	}
	if err := connection.ChainSync().Client.Sync([]pcommon.Point{point}); err != nil {
		return fail("intersection_failed", 69)
	}
	digest := sha256.Sum256(startupCanonical)
	if err := writer.write(readyEvent{
		AuthorityNodeID:       config.AuthorityNodeID,
		CurrentTip:            tip(*currentTip),
		GenesisIdentitySHA256: config.GenesisIdentitySHA256,
		Kind:                  "ready",
		Network:               config.Network,
		NetworkMagic:          config.NetworkMagic,
		Operation:             config.Operation,
		SchemaVersion:         schemaVersion,
		SelectedIntersection:  config.Intersection,
		SocketPath:            config.SocketPath,
		StartupDigest:         hex.EncodeToString(digest[:]),
	}); err != nil {
		if stopped() {
			return 0
		}
		return 74
	}
	releaseReadyGate.Do(func() { close(readyGate) })
	if onReady != nil {
		onReady()
	}

	select {
	case <-stop:
		return 0
	case err := <-errorChannel:
		if stopped() {
			return 0
		}
		_ = writeChainSyncFailure(writer, diagnostics, err)
		return 70
	}
}
