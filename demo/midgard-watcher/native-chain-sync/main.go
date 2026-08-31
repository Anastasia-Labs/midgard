package main

import (
	"bufio"
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"log/slog"
	"os"
	"os/signal"
	"path/filepath"
	"regexp"
	"sync"
	"syscall"
	"time"

	ouroboros "github.com/blinklabs-io/gouroboros"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
)

const (
	schemaVersion   = "midgard-watcher-native-chain-sync-v1"
	maxStartupBytes = 64 * 1024
	maxBlockBytes   = 4 * 1024 * 1024
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

// Fields are declared in canonical lexicographic JSON-key order.
type startupConfig struct {
	AuthorityNodeID       string    `json:"authorityNodeId"`
	GenesisIdentitySHA256 string    `json:"genesisIdentitySha256"`
	Intersection          wirePoint `json:"intersection"`
	Network               string    `json:"network"`
	NetworkMagic          uint32    `json:"networkMagic"`
	SchemaVersion         string    `json:"schemaVersion"`
	SocketPath            string    `json:"socketPath"`
}

type wireTip struct {
	BlockHash string `json:"blockHash,omitempty"`
	BlockNo   string `json:"blockNo,omitempty"`
	Kind      string `json:"kind"`
	Slot      string `json:"slot,omitempty"`
}

type readyEvent struct {
	AuthorityNodeID       string    `json:"authorityNodeId"`
	CurrentTip            wireTip   `json:"currentTip"`
	GenesisIdentitySHA256 string    `json:"genesisIdentitySha256"`
	Kind                  string    `json:"kind"`
	Network               string    `json:"network"`
	NetworkMagic          uint32    `json:"networkMagic"`
	SchemaVersion         string    `json:"schemaVersion"`
	SelectedIntersection  wirePoint `json:"selectedIntersection"`
	SocketPath            string    `json:"socketPath"`
	StartupDigest         string    `json:"startupDigest"`
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
	line, err := reader.ReadBytes('\n')
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
	if !ok || expectedMagic != config.NetworkMagic {
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
	if err := validatePoint(config.Intersection); err != nil {
		return fmt.Errorf("startup intersection is invalid: %w", err)
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

func main() {
	writer := &canonicalWriter{encoder: json.NewEncoder(os.Stdout)}
	config, startupCanonical, err := readStartup()
	if err != nil {
		_ = writer.write(errorEvent{Code: "invalid_startup", Kind: "error", SchemaVersion: schemaVersion})
		os.Exit(64)
	}

	errorChannel := make(chan error, 4)
	readyGate := make(chan struct{})
	chainSyncConfig := chainsync.Config{
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
			return writer.write(rollForwardEvent{
				BlockHash:     block.Hash().String(),
				BlockNo:       fmt.Sprintf("%d", block.BlockNumber()),
				BlockType:     fmt.Sprintf("%d", blockType),
				Kind:          "roll_forward",
				PrevHash:      block.PrevHash().String(),
				RawBlockCBOR:  hex.EncodeToString(raw),
				SchemaVersion: schemaVersion,
				Slot:          fmt.Sprintf("%d", block.SlotNumber()),
				Tip:           tip(eventTip),
			})
		},
		RollBackwardFunc: func(_ chainsync.CallbackContext, point pcommon.Point, eventTip chainsync.Tip) error {
			<-readyGate
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
	connection, err := ouroboros.New(
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
	if err := connection.DialTimeout("unix", config.SocketPath, 10*time.Second); err != nil {
		_ = writer.write(errorEvent{Code: "node_handshake_failed", Kind: "error", SchemaVersion: schemaVersion})
		os.Exit(69)
	}
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
	case <-errorChannel:
		_ = writer.write(errorEvent{Code: "chain_sync_failed", Kind: "error", SchemaVersion: schemaVersion})
		os.Exit(70)
	}
}
