package main

import (
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
	config, startupCanonical, err := readStartup()
	if err != nil {
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

	errorChannel := make(chan error, 4)
	readyGate := make(chan struct{})
	chainSyncConfig := makeChainSyncConfig(config, writer, readyGate)
	dialTimeout := 10 * time.Second
	var queryDeadline time.Time
	if config.Operation.Kind == "exact_point" {
		queryDeadline = time.Now().Add(time.Duration(config.Operation.TimeoutMs) * time.Millisecond)
		dialTimeout = min(dialTimeout, time.Until(queryDeadline))
	}
	socket, err := net.DialTimeout("unix", config.SocketPath, dialTimeout)
	if err != nil {
		_ = writer.write(errorEvent{Code: "node_handshake_failed", Kind: "error", SchemaVersion: schemaVersion})
		os.Exit(69)
	}
	defer socket.Close()
	var transport net.Conn = &streamSegmentConn{Conn: socket}
	if config.Operation.Kind == "exact_point" {
		if err := socket.SetDeadline(queryDeadline); err != nil {
			_ = writer.write(errorEvent{Code: "node_deadline_failed", Kind: "error", SchemaVersion: schemaVersion})
			os.Exit(69)
		}
		transport = &queryLimitedConn{Conn: socket, remaining: maxQueryIngressBytes, deadline: queryDeadline}
	}
	connection, err := ouroboros.New(
		ouroboros.WithConnection(transport),
		ouroboros.WithNetworkMagic(config.NetworkMagic),
		ouroboros.WithNodeToNode(false),
		ouroboros.WithErrorChan(errorChannel),
		ouroboros.WithLogger(slog.New(slog.NewJSONHandler(os.Stderr, nil))),
		ouroboros.WithChainSyncConfig(chainSyncConfig),
	)
	if err != nil {
		_ = writer.write(errorEvent{Code: "connection_setup_failed", Kind: "error", SchemaVersion: schemaVersion})
		os.Exit(70)
	}
	defer connection.Close()
	currentTip, err := connection.ChainSync().Client.GetCurrentTip()
	if err != nil {
		_ = writer.write(errorEvent{Code: "tip_query_failed", Kind: "error", SchemaVersion: schemaVersion})
		os.Exit(69)
	}
	point, err := pointFromStartup(config.Intersection)
	if err != nil {
		_ = writer.write(errorEvent{Code: "invalid_intersection", Kind: "error", SchemaVersion: schemaVersion})
		os.Exit(64)
	}
	if err := connection.ChainSync().Client.Sync([]pcommon.Point{point}); err != nil {
		_ = writer.write(errorEvent{Code: "intersection_failed", Kind: "error", SchemaVersion: schemaVersion})
		os.Exit(69)
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
		os.Exit(74)
	}
	close(readyGate)

	signals := make(chan os.Signal, 1)
	signal.Notify(signals, syscall.SIGINT, syscall.SIGTERM)
	select {
	case <-signals:
		return
	case err := <-errorChannel:
		_ = writeChainSyncFailure(writer, os.Stderr, err)
		os.Exit(70)
	}
}
