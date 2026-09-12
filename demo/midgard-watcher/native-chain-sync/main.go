package main

import (
	"bufio"
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"net"
	"os"
	"os/signal"
	"path/filepath"
	"regexp"
	"strconv"
	"sync"
	"syscall"
	"time"

	ouroboros "github.com/blinklabs-io/gouroboros"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
)

const (
	schemaVersion        = "midgard-watcher-native-chain-sync-v1"
	maxStartupBytes      = 64 * 1024
	maxBlockBytes        = 4 * 1024 * 1024
	maxQueryIngressBytes = 8 * 1024 * 1024
)

var (
	hex32Pattern = regexp.MustCompile(`^[0-9a-f]{64}$`)
	idPattern    = regexp.MustCompile(`^[a-z0-9](?:[a-z0-9._-]{0,62}[a-z0-9])?$`)
	networkMagic = map[string]uint32{
		"Mainnet": 764824073,
		"Preprod": 1,
		"Preview": 2,
	}
)

type wirePoint struct {
	BlockHash string `json:"blockHash,omitempty"`
	Kind      string `json:"kind"`
	Slot      string `json:"slot,omitempty"`
}

type wireBlockPoint struct {
	BlockHash string `json:"blockHash"`
	BlockNo   string `json:"blockNo"`
	Slot      string `json:"slot"`
}

type wireOperation struct {
	Credential         *wireStakeCredential `json:"credential,omitempty"`
	Kind               string               `json:"kind"`
	PredecessorBlockNo string               `json:"predecessorBlockNo,omitempty"`
	Target             *wireBlockPoint      `json:"target,omitempty"`
	TimeoutMs          uint64               `json:"timeoutMs,omitempty"`
}

// Fields are declared in canonical lexicographic JSON-key order.
type startupConfig struct {
	AuthorityNodeID       string        `json:"authorityNodeId"`
	GenesisIdentitySHA256 string        `json:"genesisIdentitySha256"`
	Intersection          wirePoint     `json:"intersection"`
	Network               string        `json:"network"`
	NetworkMagic          uint32        `json:"networkMagic"`
	Operation             wireOperation `json:"operation"`
	SchemaVersion         string        `json:"schemaVersion"`
	SocketPath            string        `json:"socketPath"`
}

type wireTip struct {
	BlockHash string `json:"blockHash,omitempty"`
	BlockNo   string `json:"blockNo,omitempty"`
	Kind      string `json:"kind"`
	Slot      string `json:"slot,omitempty"`
}

type readyEvent struct {
	AuthorityNodeID       string        `json:"authorityNodeId"`
	CurrentTip            wireTip       `json:"currentTip"`
	GenesisIdentitySHA256 string        `json:"genesisIdentitySha256"`
	Kind                  string        `json:"kind"`
	Network               string        `json:"network"`
	NetworkMagic          uint32        `json:"networkMagic"`
	Operation             wireOperation `json:"operation"`
	SchemaVersion         string        `json:"schemaVersion"`
	SelectedIntersection  wirePoint     `json:"selectedIntersection"`
	SocketPath            string        `json:"socketPath"`
	StartupDigest         string        `json:"startupDigest"`
}

type rollForwardEvent struct {
	BlockHash     string  `json:"blockHash"`
	BlockNo       string  `json:"blockNo"`
	BlockType     string  `json:"blockType"`
	Kind          string  `json:"kind"`
	PrevHash      string  `json:"prevHash"`
	RawBlockCBOR  string  `json:"rawBlockCbor"`
	SchemaVersion string  `json:"schemaVersion"`
	Slot          string  `json:"slot"`
	Tip           wireTip `json:"tip"`
}

type rollBackwardEvent struct {
	Kind          string    `json:"kind"`
	Point         wirePoint `json:"point"`
	SchemaVersion string    `json:"schemaVersion"`
	Tip           wireTip   `json:"tip"`
}

type errorEvent struct {
	Code          string `json:"code"`
	Kind          string `json:"kind"`
	SchemaVersion string `json:"schemaVersion"`
}

type canonicalWriter struct {
	encoder *json.Encoder
	mutex   sync.Mutex
}

func (w *canonicalWriter) write(value any) error {
	w.mutex.Lock()
	defer w.mutex.Unlock()
	w.encoder.SetEscapeHTML(false)
	return w.encoder.Encode(value)
}

func canonicalJSON(value any) ([]byte, error) {
	return json.Marshal(value)
}

func readStartup() (startupConfig, []byte, error) {
	reader := bufio.NewReaderSize(os.Stdin, maxStartupBytes+1)
	line, err := reader.ReadSlice('\n')
	if err != nil {
		return startupConfig{}, nil, fmt.Errorf("read startup: %w", err)
	}
	if len(line) < 2 || len(line) > maxStartupBytes || line[len(line)-1] != '\n' {
		return startupConfig{}, nil, errors.New("startup line size is invalid")
	}
	line = line[:len(line)-1]
	decoder := json.NewDecoder(bytes.NewReader(line))
	decoder.DisallowUnknownFields()
	var config startupConfig
	if err := decoder.Decode(&config); err != nil {
		return startupConfig{}, nil, fmt.Errorf("decode startup: %w", err)
	}
	canonical, err := canonicalJSON(config)
	if err != nil {
		return startupConfig{}, nil, err
	}
	if !bytes.Equal(line, canonical) {
		return startupConfig{}, nil, errors.New("startup line is not canonical JSON")
	}
	if err := validateStartup(config); err != nil {
		return startupConfig{}, nil, err
	}
	return config, canonical, nil
}

func validateStartup(config startupConfig) error {
	if config.SchemaVersion != schemaVersion {
		return errors.New("startup schema version is unsupported")
	}
	if !idPattern.MatchString(config.AuthorityNodeID) || !hex32Pattern.MatchString(config.GenesisIdentitySHA256) {
		return errors.New("startup authority identity is invalid")
	}
	expectedMagic, ok := networkMagic[config.Network]
	if config.Network == "Custom" {
		for _, publicMagic := range networkMagic {
			if config.NetworkMagic == publicMagic {
				return errors.New("custom network uses public network magic")
			}
		}
	} else if !ok || expectedMagic != config.NetworkMagic {
		return errors.New("startup network magic differs from named network")
	}
	if !filepath.IsAbs(config.SocketPath) || filepath.Clean(config.SocketPath) != config.SocketPath || config.SocketPath == "/" {
		return errors.New("startup socket path is invalid")
	}
	realSocket, err := filepath.EvalSymlinks(config.SocketPath)
	if err != nil || realSocket != config.SocketPath {
		return errors.New("startup socket path is absent or traverses a symlink")
	}
	info, err := os.Stat(config.SocketPath)
	if err != nil || info.Mode()&os.ModeSocket == 0 {
		return errors.New("startup path is not a Unix socket")
	}
	if err := validateOperation(config); err != nil {
		return fmt.Errorf("startup operation is invalid: %w", err)
	}
	if err := validatePoint(config.Intersection); err != nil {
		return fmt.Errorf("startup intersection is invalid: %w", err)
	}
	return nil
}

func parseUint64(value string) (uint64, error) {
	if !canonicalNatural(value) {
		return 0, errors.New("value is not a canonical UInt64")
	}
	return strconv.ParseUint(value, 10, 64)
}

func validateOperation(config startupConfig) error {
	op := config.Operation
	if op.Kind == "reward_account" {
		if op.Credential == nil || !regexp.MustCompile(`^[0-9a-f]{56}$`).MatchString(op.Credential.Hash) ||
			(op.Credential.Type != "Key" && op.Credential.Type != "Script") ||
			op.TimeoutMs < 100 || op.TimeoutMs > 120000 || op.Target != nil || op.PredecessorBlockNo != "" || config.Intersection.Kind != "origin" {
			return errors.New("reward-account operation is invalid")
		}
		return nil
	}
	if op.Credential != nil {
		return errors.New("chain-sync operation carries a reward credential")
	}
	if op.Kind == "stream" {
		if op.Target != nil || op.PredecessorBlockNo != "" || op.TimeoutMs != 0 {
			return errors.New("stream operation carries exact-query fields")
		}
		return nil
	}
	if op.Kind != "exact_point" || op.Target == nil || config.Intersection.Kind != "point" {
		return errors.New("operation must be stream or an exact point query")
	}
	if op.TimeoutMs < 100 || op.TimeoutMs > 120000 {
		return errors.New("exact-query timeout is invalid")
	}
	if !hex32Pattern.MatchString(op.Target.BlockHash) {
		return errors.New("exact-query target hash is invalid")
	}
	parentNo, err := parseUint64(op.PredecessorBlockNo)
	if err != nil {
		return fmt.Errorf("predecessor block number: %w", err)
	}
	targetNo, err := parseUint64(op.Target.BlockNo)
	if err != nil {
		return fmt.Errorf("target block number: %w", err)
	}
	parentSlot, err := parseUint64(config.Intersection.Slot)
	if err != nil {
		return fmt.Errorf("predecessor slot: %w", err)
	}
	targetSlot, err := parseUint64(op.Target.Slot)
	if err != nil {
		return fmt.Errorf("target slot: %w", err)
	}
	if targetNo == 0 || targetNo-1 != parentNo || targetSlot <= parentSlot {
		return errors.New("exact-query target is not a direct successor")
	}
	return nil
}

func validatePoint(point wirePoint) error {
	if point.Kind == "origin" {
		if point.BlockHash != "" || point.Slot != "" {
			return errors.New("origin carries point fields")
		}
		return nil
	}
	if point.Kind != "point" || !canonicalNatural(point.Slot) || !hex32Pattern.MatchString(point.BlockHash) {
		return errors.New("point fields are invalid")
	}
	return nil
}

func canonicalNatural(value string) bool {
	if value == "0" {
		return true
	}
	if len(value) == 0 || value[0] == '0' {
		return false
	}
	for _, char := range value {
		if char < '0' || char > '9' {
			return false
		}
	}
	return true
}

func pointFromStartup(point wirePoint) (pcommon.Point, error) {
	if point.Kind == "origin" {
		return pcommon.NewPointOrigin(), nil
	}
	hash, err := hex.DecodeString(point.BlockHash)
	if err != nil {
		return pcommon.Point{}, err
	}
	var slot uint64
	if _, err := fmt.Sscan(point.Slot, &slot); err != nil {
		return pcommon.Point{}, err
	}
	return pcommon.NewPoint(slot, hash), nil
}

func tip(tip chainsync.Tip) wireTip {
	if len(tip.Point.Hash) == 0 && tip.Point.Slot == 0 {
		return wireTip{Kind: "origin"}
	}
	return wireTip{
		BlockHash: hex.EncodeToString(tip.Point.Hash),
		BlockNo:   fmt.Sprintf("%d", tip.BlockNumber),
		Kind:      "point",
		Slot:      fmt.Sprintf("%d", tip.Point.Slot),
	}
}

// Each exact query owns one connection. Its ingress limit is applied to the
// actual Read slice before the Ouroboros muxer can reassemble a block frame.
type queryLimitedConn struct {
	net.Conn
	remaining int
	deadline  time.Time
}

// The muxer refreshes its segment deadline before every read. It must never
// extend the exact query's absolute operation deadline.
func (c *queryLimitedConn) SetReadDeadline(deadline time.Time) error {
	if !c.deadline.IsZero() && (deadline.IsZero() || deadline.After(c.deadline)) {
		deadline = c.deadline
	}
	return c.Conn.SetReadDeadline(deadline)
}

// The pinned muxer arms a segment deadline before reading its header. NtC may
// legitimately wait indefinitely between segments (AwaitReply or owner
// backpressure). Start that deadline at the first byte instead, retaining the
// bounded read of the remaining header and payload. Only the muxer's single
// reader calls Read and SetReadDeadline; Close still interrupts an idle read.
type streamSegmentConn struct {
	net.Conn
	segmentTimeout  time.Duration
	awaitingSegment bool
}

func (c *streamSegmentConn) SetReadDeadline(deadline time.Time) error {
	c.awaitingSegment = !deadline.IsZero()
	c.segmentTimeout = time.Until(deadline)
	return c.Conn.SetReadDeadline(time.Time{})
}

func (c *streamSegmentConn) Read(buffer []byte) (int, error) {
	if !c.awaitingSegment || len(buffer) == 0 {
		return c.Conn.Read(buffer)
	}
	n, err := c.Conn.Read(buffer[:1])
	if n > 0 {
		c.awaitingSegment = false
		if deadlineErr := c.Conn.SetReadDeadline(time.Now().Add(c.segmentTimeout)); deadlineErr != nil && err == nil {
			err = deadlineErr
		}
	}
	return n, err
}

func (c *queryLimitedConn) Read(buffer []byte) (int, error) {
	if c.remaining == 0 {
		return 0, errors.New("native exact-query ingress bound exceeded")
	}
	if len(buffer) > c.remaining {
		buffer = buffer[:c.remaining]
	}
	n, err := c.Conn.Read(buffer)
	c.remaining -= n
	return n, err
}

type exactQueryState struct {
	config       startupConfig
	events       int
	acknowledged bool
	captured     bool
}

func (q *exactQueryState) checkBackward(point pcommon.Point) error {
	if q.config.Operation.Kind != "exact_point" {
		return nil
	}
	q.events++
	expected, err := pointFromStartup(q.config.Intersection)
	if err != nil {
		return err
	}
	if q.captured || q.acknowledged || q.events > 2 || point.Slot != expected.Slot || !bytes.Equal(point.Hash, expected.Hash) {
		return errors.New("native exact-query rolled back outside initial intersection acknowledgement")
	}
	q.acknowledged = true
	return nil
}

func (q *exactQueryState) checkForward(block ledger.Block) error {
	if q.config.Operation.Kind != "exact_point" {
		return nil
	}
	q.events++
	target := q.config.Operation.Target
	if q.captured || q.events > 2 || target == nil ||
		block.Hash().String() != target.BlockHash ||
		fmt.Sprintf("%d", block.SlotNumber()) != target.Slot ||
		fmt.Sprintf("%d", block.BlockNumber()) != target.BlockNo ||
		block.PrevHash().String() != q.config.Intersection.BlockHash {
		return errors.New("native exact-query returned a different target or an extra block")
	}
	q.captured = true
	return nil
}

func makeChainSyncConfig(config startupConfig, writer *canonicalWriter, readyGate <-chan struct{}) chainsync.Config {
	query := exactQueryState{config: config}
	result := chainsync.Config{
		PipelineLimit: 1,
		RecvQueueSize: 4,
		RollForwardRawFunc: func(_ chainsync.CallbackContext, blockType uint, raw []byte, eventTip chainsync.Tip) error {
			<-readyGate
			if len(raw) == 0 || len(raw) > maxBlockBytes {
				return errors.New("native chain-sync block size is invalid")
			}
			block, err := ledger.NewBlockFromCbor(blockType, raw)
			if err != nil {
				return fmt.Errorf("decode native chain-sync block: %w", err)
			}
			if err := query.checkForward(block); err != nil {
				return err
			}
			prevHash := block.PrevHash().String()
			if block.BlockNumber() == 0 && prevHash == fmt.Sprintf("%064d", 0) {
				prevHash = ""
			}
			if err := writer.write(rollForwardEvent{
				BlockHash:     block.Hash().String(),
				BlockNo:       fmt.Sprintf("%d", block.BlockNumber()),
				BlockType:     fmt.Sprintf("%d", blockType),
				Kind:          "roll_forward",
				PrevHash:      prevHash,
				RawBlockCBOR:  hex.EncodeToString(raw),
				SchemaVersion: schemaVersion,
				Slot:          fmt.Sprintf("%d", block.SlotNumber()),
				Tip:           tip(eventTip),
			}); err != nil {
				return err
			}
			if config.Operation.Kind == "exact_point" {
				// PipelineLimit=1 leaves no later request outstanding. Returning
				// false through ErrStopSyncProcess stops the client's syncLoop;
				// it does not suspend a callback or close this connection.
				return chainsync.ErrStopSyncProcess
			}
			return nil
		},
		RollBackwardFunc: func(_ chainsync.CallbackContext, point pcommon.Point, eventTip chainsync.Tip) error {
			<-readyGate
			if err := query.checkBackward(point); err != nil {
				return err
			}
			rollbackPoint := wirePoint{Kind: "origin"}
			if len(point.Hash) > 0 || point.Slot != 0 {
				rollbackPoint = wirePoint{
					BlockHash: hex.EncodeToString(point.Hash),
					Kind:      "point",
					Slot:      fmt.Sprintf("%d", point.Slot),
				}
			}
			return writer.write(rollBackwardEvent{
				Kind:          "roll_backward",
				Point:         rollbackPoint,
				SchemaVersion: schemaVersion,
				Tip:           tip(eventTip),
			})
		},
	}
	if config.Operation.Kind == "exact_point" {
		result.RecvQueueSize = 1
	}
	return result
}

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
