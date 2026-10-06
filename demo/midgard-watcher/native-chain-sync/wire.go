package main

import (
	"bufio"
	"bytes"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"sync"
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
	sealed  bool
}

var errWriterSealed = errors.New("native chain-sync output is sealed")

func (w *canonicalWriter) write(value any) error {
	w.mutex.Lock()
	defer w.mutex.Unlock()
	if w.sealed {
		return errWriterSealed
	}
	w.encoder.SetEscapeHTML(false)
	return w.encoder.Encode(value)
}

// seal ends the session's stdout: callbacks still running on an abandoned
// connection cannot emit chain-sync data after the owner stopped the session.
func (w *canonicalWriter) seal() {
	w.mutex.Lock()
	defer w.mutex.Unlock()
	w.sealed = true
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
	return parseStartupLine(line[:len(line)-1])
}

// parseStartupLine admits one startup line without its newline terminator.
func parseStartupLine(line []byte) (startupConfig, []byte, error) {
	if len(line) < 1 || len(line)+1 > maxStartupBytes {
		return startupConfig{}, nil, errors.New("startup line size is invalid")
	}
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
