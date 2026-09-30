package main

import (
	"bytes"
	"encoding/hex"
	"encoding/json"
	"errors"
	ouroboros "github.com/blinklabs-io/gouroboros"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
	"net"
	"os"
	"strconv"
	"strings"
	"sync/atomic"
	"testing"
	"time"
)

func exactFixture(t *testing.T) (startupConfig, []byte, ledger.Block) {
	t.Helper()
	encoded, err := os.ReadFile("../tests/support/conway-block.hex")
	if err != nil {
		t.Fatal(err)
	}
	raw, err := hex.DecodeString(strings.TrimSpace(string(encoded)))
	if err != nil {
		t.Fatal(err)
	}
	block, err := ledger.NewBlockFromCbor(7, raw)
	if err != nil {
		t.Fatal(err)
	}
	config := validStartup(t)
	config.Intersection = wirePoint{Kind: "point", BlockHash: block.PrevHash().String(), Slot: strconv.FormatUint(block.SlotNumber()-1, 10)}
	config.Operation = wireOperation{Kind: "exact_point", PredecessorBlockNo: strconv.FormatUint(block.BlockNumber()-1, 10), Target: &wireBlockPoint{BlockHash: block.Hash().String(), BlockNo: strconv.FormatUint(block.BlockNumber(), 10), Slot: strconv.FormatUint(block.SlotNumber(), 10)}, TimeoutMs: 2000}
	return config, raw, block
}

func TestExactQueryBounds(t *testing.T) {
	config, _, block := exactFixture(t)
	if err := validateStartup(config); err != nil {
		t.Fatal(err)
	}
	for _, mutate := range []func(*startupConfig){
		func(c *startupConfig) { c.Operation.Kind = "" },
		func(c *startupConfig) { c.Operation.TimeoutMs = 99 },
		func(c *startupConfig) { c.Operation.TimeoutMs = 120001 },
		func(c *startupConfig) { c.Operation.PredecessorBlockNo = "18446744073709551616" },
		func(c *startupConfig) { c.Operation.PredecessorBlockNo = "0" },
		func(c *startupConfig) { c.Intersection.Slot = c.Operation.Target.Slot },
		func(c *startupConfig) { c.Operation.Kind = "stream" },
	} {
		changed := config
		mutate(&changed)
		if validateStartup(changed) == nil {
			t.Fatal("invalid exact-query configuration admitted")
		}
	}
	point, _ := pointFromStartup(config.Intersection)
	query := exactQueryState{config: config}
	if err := query.checkBackward(point); err != nil {
		t.Fatal(err)
	}
	if err := query.checkForward(block); err != nil {
		t.Fatal(err)
	}
	if query.checkForward(block) == nil || query.checkBackward(point) == nil {
		t.Fatal("post-target event admitted")
	}
	other := config
	other.Intersection.BlockHash = repeat("00", 32)
	if (&exactQueryState{config: other}).checkForward(block) == nil {
		t.Fatal("different parent admitted")
	}
	repeated := exactQueryState{config: config}
	if err := repeated.checkBackward(point); err != nil {
		t.Fatal(err)
	}
	if repeated.checkBackward(point) == nil {
		t.Fatal("second acknowledgement admitted")
	}
}

type readSizeConn struct {
	net.Conn
	sizes []int
}

func (c *readSizeConn) Read(p []byte) (int, error) {
	c.sizes = append(c.sizes, len(p))
	return len(p), nil
}
func TestExactQueryIngressBoundBeforeRead(t *testing.T) {
	transport := &readSizeConn{}
	limited := &queryLimitedConn{Conn: transport, remaining: 3}
	if n, err := limited.Read(make([]byte, 100)); n != 3 || err != nil {
		t.Fatalf("read: %d %v", n, err)
	}
	if n, err := limited.Read(make([]byte, 100)); n != 0 || err == nil {
		t.Fatalf("bound: %d %v", n, err)
	}
	if len(transport.sizes) != 1 || transport.sizes[0] != 3 {
		t.Fatalf("underlying read sizes: %v", transport.sizes)
	}
}
func TestExactQueryConnectionDeadlineAndClose(t *testing.T) {
	for _, cancel := range []bool{false, true} {
		left, right := net.Pipe()
		limited := &queryLimitedConn{Conn: left, remaining: 100}
		if err := limited.SetDeadline(time.Now().Add(30 * time.Millisecond)); err != nil {
			t.Fatal(err)
		}
		if cancel {
			go limited.Close()
		}
		_, err := limited.Read(make([]byte, 10))
		_ = left.Close()
		_ = right.Close()
		if err == nil {
			t.Fatal("blocked read outlived deadline/close")
		}
	}
}

type eventWriter struct{ events chan []byte }

func (w *eventWriter) Write(p []byte) (int, error) { w.events <- bytes.Clone(p); return len(p), nil }

func TestExactQueryStopsActualRequestNextAfterTarget(t *testing.T) {
	for _, acknowledge := range []bool{false, true} {
		t.Run(strconv.FormatBool(acknowledge), func(t *testing.T) {
			config, raw, block := exactFixture(t)
			parent, _ := pointFromStartup(config.Intersection)
			tip := chainsync.Tip{Point: pcommon.NewPoint(block.SlotNumber()+5000, block.Hash().Bytes()), BlockNumber: block.BlockNumber() + 5000}
			left, right := net.Pipe()
			_ = left.SetDeadline(time.Now().Add(3 * time.Second))
			_ = right.SetDeadline(time.Now().Add(3 * time.Second))
			defer left.Close()
			defer right.Close()
			var requests atomic.Int32
			serverReady := make(chan *ouroboros.Connection, 1)
			serverErrors := make(chan error, 4)
			go func() {
				server, err := ouroboros.New(ouroboros.WithConnection(right), ouroboros.WithServer(true), ouroboros.WithNodeToNode(false), ouroboros.WithNetworkMagic(1), ouroboros.WithErrorChan(serverErrors), ouroboros.WithChainSyncConfig(chainsync.Config{
					FindIntersectFunc: func(_ chainsync.CallbackContext, points []pcommon.Point) (pcommon.Point, chainsync.Tip, error) {
						if len(points) == 0 {
							return pcommon.Point{}, tip, chainsync.ErrIntersectNotFound
						}
						return parent, tip, nil
					},
					RequestNextFunc: func(ctx chainsync.CallbackContext) error {
						count := requests.Add(1)
						if acknowledge && count == 1 {
							return ctx.Server.RollBackward(parent, tip)
						}
						return ctx.Server.RollForward(7, raw, tip)
					},
				}))
				if err != nil {
					serverErrors <- err
					return
				}
				serverReady <- server
			}()
			output := &eventWriter{events: make(chan []byte, 4)}
			gate := make(chan struct{})
			close(gate)
			clientErrors := make(chan error, 4)
			client, err := ouroboros.New(ouroboros.WithConnection(left), ouroboros.WithNodeToNode(false), ouroboros.WithNetworkMagic(1), ouroboros.WithErrorChan(clientErrors), ouroboros.WithChainSyncConfig(makeChainSyncConfig(config, &canonicalWriter{encoder: json.NewEncoder(output)}, gate)))
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
				t.Fatal("server startup timeout")
			}
			if _, err := client.ChainSync().Client.GetCurrentTip(); err != nil {
				t.Fatal(err)
			}
			if err := client.ChainSync().Client.Sync([]pcommon.Point{parent}); err != nil {
				t.Fatal(err)
			}
			expected := int32(1)
			if acknowledge {
				expected = 2
			}
			for range expected {
				select {
				case data := <-output.events:
					var event map[string]any
					if err := json.Unmarshal(data, &event); err != nil {
						t.Fatal(err)
					}
				case err := <-clientErrors:
					t.Fatal(err)
				case <-time.After(time.Second):
					t.Fatal("target callback timeout")
				}
			}
			// The peer counts decoded protocol requests while the target receipt stays idle.
			// Waiting here permits the sync loop to consume the callback's stop signal.
			time.Sleep(100 * time.Millisecond)
			if got := requests.Load(); got != expected {
				t.Fatalf("RequestNext count after target: got %d want %d", got, expected)
			}
			select {
			case err := <-clientErrors:
				t.Fatalf("idle client failed: %v", err)
			default:
			}
		})
	}
}

func TestExactQueryCanonicalStartupAndInputBound(t *testing.T) {
	config, _, _ := exactFixture(t)
	canonical, err := canonicalJSON(config)
	if err != nil {
		t.Fatal(err)
	}
	var generic any
	if err := json.Unmarshal(canonical, &generic); err != nil {
		t.Fatal(err)
	}
	sorted, err := json.Marshal(generic)
	if err != nil || !bytes.Equal(canonical, sorted) {
		t.Fatal("startup is not canonical lexicographic JSON")
	}
	original := os.Stdin
	defer func() { os.Stdin = original }()
	for _, sample := range []struct {
		line  []byte
		valid bool
	}{
		{append(bytes.Clone(canonical), '\n'), true},
		{append(bytes.Replace(canonical, []byte(`"kind":"exact_point"`), []byte(`"kind":"stream"`), 1), '\n'), false},
		{append(bytes.Repeat([]byte(" "), maxStartupBytes+1), '\n'), false},
	} {
		file, err := os.CreateTemp(t.TempDir(), "startup")
		if err != nil {
			t.Fatal(err)
		}
		if _, err = file.Write(sample.line); err != nil {
			t.Fatal(err)
		}
		if _, err = file.Seek(0, 0); err != nil {
			t.Fatal(err)
		}
		os.Stdin = file
		_, encoded, err := readStartup()
		_ = file.Close()
		if sample.valid {
			if err != nil || !bytes.Equal(encoded, canonical) {
				t.Fatalf("canonical startup refused: %v", err)
			}
		} else if err == nil {
			t.Fatal("invalid startup admitted")
		}
	}
}

func TestChainSyncFailurePreservesCauseAndExactWireSchema(t *testing.T) {
	var stdout, stderr bytes.Buffer
	writer := &canonicalWriter{encoder: json.NewEncoder(&stdout)}
	if err := writeChainSyncFailure(writer, &stderr, errors.New("decode native chain-sync block: retained concrete cause")); err != nil {
		t.Fatal(err)
	}
	if stderr.String() != "native chain-sync failed: decode native chain-sync block: retained concrete cause\n" {
		t.Fatalf("lost native cause: %q", stderr.String())
	}
	expected := "{\"code\":\"chain_sync_failed\",\"kind\":\"error\",\"schemaVersion\":\"" + schemaVersion + "\"}\n"
	if stdout.String() != expected {
		t.Fatalf("terminal schema changed: %q", stdout.String())
	}
}

// These tests exercise the pinned muxer's real 120-second segment deadline.
// Virtual time avoids changing production constants or waiting for real slots.
