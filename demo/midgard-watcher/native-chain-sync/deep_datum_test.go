package main

import (
	"bytes"
	"encoding/hex"
	"encoding/json"
	"net"
	"os"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	ouroboros "github.com/blinklabs-io/gouroboros"
	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
	"golang.org/x/crypto/blake2b"
)

func encodeDeepDatumFixture(t *testing.T, datum []byte) []byte {
	t.Helper()
	fixture, err := os.ReadFile("../tests/support/conway-block.hex")
	if err != nil {
		t.Fatal(err)
	}
	raw, err := hex.DecodeString(strings.TrimSpace(string(fixture)))
	if err != nil {
		t.Fatal(err)
	}
	decode := func(raw []byte, value any) {
		t.Helper()
		if _, err := cbor.Decode(raw, value); err != nil {
			t.Fatal(err)
		}
	}
	encode := func(value any) cbor.RawMessage {
		t.Helper()
		raw, err := cbor.Encode(value)
		if err != nil {
			t.Fatal(err)
		}
		return raw
	}
	var block, witnesses, header, headerBody []cbor.RawMessage
	decode(raw, &block)
	decode(block[2], &witnesses)
	var witness map[uint64]cbor.RawMessage
	decode(witnesses[0], &witness)
	// Conway witness field 4 is the Plutus datum collection. Reuse the real
	// block's transaction/header shapes and replace only its first datum set.
	witness[4] = encode([]cbor.RawMessage{datum})
	witnesses[0] = encode(witness)
	block[2] = encode(witnesses)
	decode(block[0], &header)
	decode(header[0], &headerBody)
	// Cardano hashes the four encoded body components separately and then
	// hashes their concatenation. Preserve that binding in this synthetic
	// parser fixture; no signature/ledger-validity claim is made for it.
	var hashes []byte
	var bodySize uint64
	for _, part := range block[1:] {
		hash := blake2b.Sum256(part)
		hashes = append(hashes, hash[:]...)
		bodySize += uint64(len(part))
	}
	bodyHash := blake2b.Sum256(hashes)
	headerBody[6] = encode(bodySize)
	headerBody[7] = encode(bodyHash[:])
	header[0] = encode(headerBody)
	block[0] = encode(header)
	return encode(block)
}

func nestedPlutusList(depth int, leaf byte) []byte {
	// Each 0x81 is a one-element definite Data list. 300 levels occupy only
	// 301 datum bytes, well inside a normal Cardano transaction's wire bound.
	return append(bytes.Repeat([]byte{0x81}, depth), leaf)
}

func TestNativeStreamProgressesPastDeepPlutusDatum(t *testing.T) {
	var output bytes.Buffer
	ready := make(chan struct{})
	close(ready)
	callbacks := makeChainSyncConfig(startupConfig{Operation: wireOperation{Kind: "stream"}},
		&canonicalWriter{encoder: json.NewEncoder(&output)}, ready)
	for _, depth := range []int{300, 1000} {
		raw := encodeDeepDatumFixture(t, nestedPlutusList(depth, 0))
		if err := callbacks.RollForwardRawFunc(chainsync.CallbackContext{}, 7, raw, chainsync.Tip{}); err != nil {
			t.Fatalf("native stream rejected supported %d-level Plutus datum: %v", depth, err)
		}
		var event rollForwardEvent
		if err := json.NewDecoder(&output).Decode(&event); err != nil {
			t.Fatal(err)
		}
		if event.Kind != "roll_forward" || event.RawBlockCBOR != hex.EncodeToString(raw) {
			t.Fatal("native stream did not preserve the deep-datum block")
		}
		block, err := ledger.NewBlockFromCbor(7, raw)
		if err != nil {
			t.Fatal(err)
		}
		if event.BlockHash != block.Hash().String() || event.PrevHash != block.PrevHash().String() {
			t.Fatal("native stream lost the parsed header identity")
		}
	}
	// The next ordinary block also reaches the writer; the consumer never
	// skips, substitutes, or retries forever on the earlier deep datum.
	fixture, err := os.ReadFile("../tests/support/conway-block.hex")
	if err != nil {
		t.Fatal(err)
	}
	raw, err := hex.DecodeString(strings.TrimSpace(string(fixture)))
	if err != nil {
		t.Fatal(err)
	}
	if err := callbacks.RollForwardRawFunc(chainsync.CallbackContext{}, 7, raw, chainsync.Tip{}); err != nil {
		t.Fatal(err)
	}
	var event rollForwardEvent
	if err := json.NewDecoder(&output).Decode(&event); err != nil || event.RawBlockCBOR != hex.EncodeToString(raw) {
		t.Fatalf("stream did not advance to the next block: %v", err)
	}
}

func TestNativeStreamRejectsMalformedDeepDatum(t *testing.T) {
	var output bytes.Buffer
	ready := make(chan struct{})
	close(ready)
	callbacks := makeChainSyncConfig(startupConfig{Operation: wireOperation{Kind: "stream"}},
		&canonicalWriter{encoder: json.NewEncoder(&output)}, ready)
	// 0x1c uses a reserved CBOR integer additional-info value. Raising the
	// depth limit must not turn an invalid datum into an emitted block.
	datum := nestedPlutusList(300, 0)
	raw := encodeDeepDatumFixture(t, datum)
	position := bytes.Index(raw, datum)
	if position < 0 {
		t.Fatal("fixture lost the datum bytes")
	}
	raw[position+len(datum)-1] = 0x1c
	err := callbacks.RollForwardRawFunc(chainsync.CallbackContext{}, 7, raw, chainsync.Tip{})
	if err == nil || !strings.Contains(err.Error(), "decode native chain-sync block") {
		t.Fatalf("malformed deep datum was not refused by the native decoder: %v", err)
	}
	if output.Len() != 0 {
		t.Fatal("malformed datum produced a canonical stream event")
	}
}

func TestNativeStreamReceivesDeepDatumFromPeer(t *testing.T) {
	deep := encodeDeepDatumFixture(t, nestedPlutusList(300, 0))
	ordinary := encodeDeepDatumFixture(t, nestedPlutusList(1, 0))
	block, err := ledger.NewBlockFromCbor(7, ordinary)
	if err != nil {
		t.Fatal(err)
	}
	parent := pcommon.NewPoint(block.SlotNumber()-1, block.PrevHash().Bytes())
	tip := chainsync.Tip{Point: pcommon.NewPoint(block.SlotNumber(), block.Hash().Bytes()), BlockNumber: block.BlockNumber()}
	left, right := net.Pipe()
	defer left.Close()
	defer right.Close()
	_ = left.SetDeadline(time.Now().Add(3 * time.Second))
	_ = right.SetDeadline(time.Now().Add(3 * time.Second))
	serverReady := make(chan *ouroboros.Connection, 1)
	serverErrors := make(chan error, 4)
	var requests atomic.Int32
	go func() {
		server, err := ouroboros.New(ouroboros.WithConnection(right), ouroboros.WithServer(true),
			ouroboros.WithNodeToNode(false), ouroboros.WithNetworkMagic(1), ouroboros.WithErrorChan(serverErrors),
			ouroboros.WithChainSyncConfig(chainsync.Config{
				FindIntersectFunc: func(_ chainsync.CallbackContext, _ []pcommon.Point) (pcommon.Point, chainsync.Tip, error) {
					return parent, tip, nil
				},
				RequestNextFunc: func(ctx chainsync.CallbackContext) error {
					switch requests.Add(1) {
					case 1:
						return ctx.Server.RollForward(7, deep, tip)
					case 2:
						return ctx.Server.RollForward(7, ordinary, tip)
					default:
						return ctx.Server.AwaitReply()
					}
				},
			}))
		if err != nil {
			serverErrors <- err
			return
		}
		serverReady <- server
	}()
	output := &eventWriter{events: make(chan []byte, 4)}
	ready := make(chan struct{})
	close(ready)
	clientErrors := make(chan error, 4)
	client, err := ouroboros.New(ouroboros.WithConnection(left), ouroboros.WithNodeToNode(false),
		ouroboros.WithNetworkMagic(1), ouroboros.WithErrorChan(clientErrors),
		ouroboros.WithChainSyncConfig(makeChainSyncConfig(startupConfig{Operation: wireOperation{Kind: "stream"}},
			&canonicalWriter{encoder: json.NewEncoder(output)}, ready)))
	if err != nil {
		t.Fatal(err)
	}
	defer client.Close()
	select {
	case server := <-serverReady:
		defer server.Close()
	case err := <-serverErrors:
		t.Fatal(err)
	case <-time.After(time.Second):
		t.Fatal("native peer did not complete the handshake")
	}
	if err := client.ChainSync().Client.Sync([]pcommon.Point{parent}); err != nil {
		t.Fatal(err)
	}
	for _, expected := range [][]byte{deep, ordinary} {
		select {
		case raw := <-output.events:
			var event rollForwardEvent
			if err := json.Unmarshal(raw, &event); err != nil || event.RawBlockCBOR != hex.EncodeToString(expected) {
				t.Fatalf("native peer stream lost or reordered a block: %v", err)
			}
		case err := <-clientErrors:
			t.Fatalf("native peer stream stopped on deep datum: %v", err)
		case <-time.After(time.Second):
			t.Fatal("native peer stream did not advance beyond deep datum")
		}
	}
}
