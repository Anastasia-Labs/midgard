package main

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"net"
	"path/filepath"
	"syscall"
	"testing"
	"time"

	"github.com/anastasia-labs/midgard-l1-node-transport/mocknode"
	gcbor "github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/muxer"
	"github.com/blinklabs-io/gouroboros/protocol/handshake"
	"github.com/blinklabs-io/gouroboros/protocol/localtxmonitor"
	"github.com/fxamacker/cbor/v2"
)

const testMagic = 42

type received struct {
	header  map[string]any
	payload []byte
}

type harness struct {
	t      *testing.T
	node   *mocknode.Node
	socket string
	in     *io.PipeWriter
	frames chan received
	status chan int
	stop   chan struct{}
	nextID uint64
}

func startNode(t *testing.T) (*mocknode.Node, string) {
	t.Helper()
	socket := filepath.Join(t.TempDir(), "node.socket")
	node, err := mocknode.Start(socket, testMagic)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(node.Close)
	return node, socket
}

// startSidecar runs one session in-process over pipes.
func startSidecar(t *testing.T, node *mocknode.Node, socket string) *harness {
	t.Helper()
	inR, inW := io.Pipe()
	outR, outW := io.Pipe()
	h := &harness{
		t: t, node: node, socket: socket, in: inW,
		frames: make(chan received, 1024), status: make(chan int, 1), stop: make(chan struct{}),
	}
	go func() {
		h.status <- runSession(inR, outW, io.Discard, h.stop)
		_ = outW.Close()
	}()
	go func() {
		defer close(h.frames)
		for {
			f, err := readFrame(outR)
			if err != nil {
				return
			}
			var header map[string]any
			if err := cbor.Unmarshal(f.header, &header); err != nil {
				t.Errorf("sidecar wrote an undecodable header: %v", err)
				return
			}
			h.frames <- received{header: header, payload: f.payload}
		}
	}()
	t.Cleanup(func() {
		_ = inW.Close()
		select {
		case <-h.status:
		case <-time.After(5 * time.Second):
			t.Error("sidecar did not end after its input closed")
		}
	})
	return h
}

func (h *harness) sendRaw(header any, payload []byte) {
	h.t.Helper()
	encoded, err := cbor.Marshal(header)
	if err != nil {
		h.t.Fatal(err)
	}
	var lengths [8]byte
	binary.BigEndian.PutUint32(lengths[0:4], uint32(len(encoded)))
	binary.BigEndian.PutUint32(lengths[4:8], uint32(len(payload)))
	if _, err := h.in.Write(append(append(lengths[:], encoded...), payload...)); err != nil {
		h.t.Fatal(err)
	}
}

func (h *harness) id() uint64 {
	h.nextID++
	return h.nextID
}

func (h *harness) next() received {
	h.t.Helper()
	select {
	case f, ok := <-h.frames:
		if !ok {
			h.t.Fatal("sidecar output ended")
		}
		return f
	case <-time.After(5 * time.Second):
		h.t.Fatal("no frame from the sidecar")
	}
	return received{}
}

// quiet asserts that no frame arrives for d.
func (h *harness) quiet(d time.Duration) {
	h.t.Helper()
	select {
	case f, ok := <-h.frames:
		if ok {
			h.t.Fatalf("unexpected frame %v", f.header)
		}
	case <-time.After(d):
	}
}

func (h *harness) expect(kind string) received {
	h.t.Helper()
	f := h.next()
	if f.header["type"] != kind {
		h.t.Fatalf("expected %s, got %v", kind, f.header)
	}
	return f
}

// testDeadlineMs is the request deadline of a test session's hello.
const testDeadlineMs = 10_000

func (h *harness) hello() {
	h.t.Helper()
	h.helloWithDeadline(testDeadlineMs)
}

func (h *harness) helloWithDeadline(deadlineMs uint64) {
	h.t.Helper()
	h.sendRaw(map[string]any{"type": "hello", "version": 1, "socketPath": h.socket, "networkMagic": testMagic, "requestDeadlineMs": deadlineMs}, nil)
	h.expect("hello_ok")
}

func point(b mocknode.Block) []any { return []any{b.Slot, b.Hash} }

func asUint(t *testing.T, v any) uint64 {
	t.Helper()
	n, ok := v.(uint64)
	if !ok {
		t.Fatalf("not an unsigned integer: %#v", v)
	}
	return n
}

func samePoint(t *testing.T, got any, want mocknode.Block) {
	t.Helper()
	parts, ok := got.([]any)
	if !ok || len(parts) != 2 || asUint(t, parts[0]) != want.Slot || !bytes.Equal(parts[1].([]byte), want.Hash) {
		t.Fatalf("point %#v is not block %d", got, want.Number)
	}
}

func (h *harness) open(stream uint64, points []any, startSeq uint64, window uint64) received {
	h.t.Helper()
	h.sendRaw(map[string]any{"type": "cs_open", "id": h.id(), "stream": stream, "points": points, "startSeq": startSeq, "window": window}, nil)
	return h.next()
}

func (h *harness) forward(stream, seq uint64, block mocknode.Block) {
	h.t.Helper()
	f := h.expect("cs_roll_forward")
	if asUint(h.t, f.header["stream"]) != stream || asUint(h.t, f.header["seq"]) != seq {
		h.t.Fatalf("expected stream %d seq %d, got %v", stream, seq, f.header)
	}
	samePoint(h.t, f.header["point"], block)
	if asUint(h.t, f.header["blockNo"]) != block.Number || asUint(h.t, f.header["blockType"]) != mocknode.BlockType {
		h.t.Fatalf("block identity mismatch: %v", f.header)
	}
	if !bytes.Equal(f.header["prevHash"].([]byte), block.Prev) {
		h.t.Fatal("prevHash mismatch")
	}
	if !bytes.Equal(f.payload, block.Raw) {
		h.t.Fatal("raw block bytes did not round-trip")
	}
}

func (h *harness) ack(stream, seq uint64) {
	h.sendRaw(map[string]any{"type": "cs_ack", "stream": stream, "seq": seq}, nil)
}

func extend(t *testing.T, node *mocknode.Node, count int, branch uint64) []mocknode.Block {
	t.Helper()
	blocks, err := node.Extend(count, branch)
	if err != nil {
		t.Fatal(err)
	}
	return blocks
}

func TestHelloRefusesUnsupportedVersionAndMalformedFirstFrame(t *testing.T) {
	node, socket := startNode(t)
	h := startSidecar(t, node, socket)
	h.sendRaw(map[string]any{"type": "hello", "version": 2, "socketPath": socket, "networkMagic": testMagic}, nil)
	if f := h.expect("fatal"); f.header["code"] != "version_unsupported" {
		t.Fatalf("got %v", f.header)
	}
	if status := <-h.status; status != exitClientMisuse {
		t.Fatalf("status %d", status)
	}
	h.status <- 0

	h2 := startSidecar(t, node, socket)
	h2.sendRaw(map[string]any{"type": "cs_ack", "stream": 1, "seq": 0}, nil)
	if f := h2.expect("fatal"); f.header["code"] != "malformed_frame" {
		t.Fatalf("got %v", f.header)
	}
	h2.status <- <-h2.status

	h3 := startSidecar(t, node, socket)
	h3.sendRaw(map[string]any{"type": "hello", "version": 1, "socketPath": socket, "networkMagic": testMagic, "requestDeadlineMs": testDeadlineMs, "extra": 1}, nil)
	if f := h3.expect("fatal"); f.header["code"] != "malformed_frame" {
		t.Fatalf("unknown hello key admitted: %v", f.header)
	}
	h3.status <- <-h3.status

	for _, deadline := range []any{nil, 0} {
		h4 := startSidecar(t, node, socket)
		hello := map[string]any{"type": "hello", "version": 1, "socketPath": socket, "networkMagic": testMagic}
		if deadline != nil {
			hello["requestDeadlineMs"] = deadline
		}
		h4.sendRaw(hello, nil)
		if f := h4.expect("fatal"); f.header["code"] != "malformed_frame" {
			t.Fatalf("hello with request deadline %v admitted: %v", deadline, f.header)
		}
		h4.status <- <-h4.status
	}
}

func TestHelloReportsUnreachableNode(t *testing.T) {
	socket := filepath.Join(t.TempDir(), "absent.socket")
	h := startSidecar(t, nil, socket)
	h.sendRaw(map[string]any{"type": "hello", "version": 1, "socketPath": socket, "networkMagic": testMagic, "requestDeadlineMs": testDeadlineMs}, nil)
	if f := h.expect("fatal"); f.header["code"] != "node_unreachable" {
		t.Fatalf("got %v", f.header)
	}
	if status := <-h.status; status != exitNodeAvailable {
		t.Fatalf("status %d", status)
	}
	h.status <- 0
}

// A node that refuses the handshake (here, another network magic) is a
// fault no restart repairs: node_handshake_failed.
func TestHelloReportsARefusedHandshake(t *testing.T) {
	socket := filepath.Join(t.TempDir(), "node.socket")
	node, err := mocknode.Start(socket, testMagic+1)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(node.Close)
	h := startSidecar(t, node, socket)
	h.sendRaw(map[string]any{"type": "hello", "version": 1, "socketPath": socket, "networkMagic": testMagic, "requestDeadlineMs": testDeadlineMs}, nil)
	if f := h.expect("fatal"); f.header["code"] != "node_handshake_failed" {
		t.Fatalf("got %v", f.header)
	}
	if status := <-h.status; status != exitNodeAvailable {
		t.Fatalf("status %d", status)
	}
	h.status <- 0
}

// A node that accepts the socket and drops it before it answers the
// handshake (a node going down) is transient: node_connection_lost.
func TestHelloReportsAConnectionDroppedDuringTheHandshake(t *testing.T) {
	socket := filepath.Join(t.TempDir(), "node.socket")
	listener, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = listener.Close() })
	go func() {
		for {
			conn, err := listener.Accept()
			if err != nil {
				return
			}
			_ = conn.Close()
		}
	}()
	h := startSidecar(t, nil, socket)
	h.sendRaw(map[string]any{"type": "hello", "version": 1, "socketPath": socket, "networkMagic": testMagic, "requestDeadlineMs": testDeadlineMs}, nil)
	if f := h.expect("fatal"); f.header["code"] != "node_connection_lost" {
		t.Fatalf("got %v", f.header)
	}
	if status := <-h.status; status != exitNodeAvailable {
		t.Fatalf("status %d", status)
	}
	h.status <- 0
}

func TestHandshakeFailureCodeSeparatesRefusalsFromConnectionDrops(t *testing.T) {
	for name, tc := range map[string]struct {
		err  error
		code string
	}{
		"version mismatch":   {&handshake.VersionMismatchError{SupportedVersions: []uint16{16}}, "node_handshake_failed"},
		"refused":            {fmt.Errorf("wrapped: %w", &handshake.RefusedError{Version: 32784, Message: "magic"}), "node_handshake_failed"},
		"decode error":       {&handshake.DecodeError{Version: 32784, Message: "bad"}, "node_handshake_failed"},
		"out of protocol":    {errors.New("handshake: received unexpected message type 9"), "node_handshake_failed"},
		"shutdown":           {fmt.Errorf("connection shutdown initiated: %w", io.EOF), "node_connection_lost"},
		"closed by the peer": {&muxer.ConnectionClosedError{Context: "reading header", Err: io.EOF}, "node_connection_lost"},
		"reset":              {&net.OpError{Op: "read", Net: "unix", Err: syscall.ECONNRESET}, "node_connection_lost"},
		"broken pipe":        {fmt.Errorf("muxer error: %w", syscall.EPIPE), "node_connection_lost"},
	} {
		if got := handshakeFailureCode(tc.err); got != tc.code {
			t.Errorf("%s: got %s, want %s", name, got, tc.code)
		}
	}
}

func TestIntersectsOnTheFirstKnownPointOfTheList(t *testing.T) {
	node, socket := startNode(t)
	blocks := extend(t, node, 5, 0)
	h := startSidecar(t, node, socket)
	h.hello()
	unknown := []any{uint64(999), bytes.Repeat([]byte{1}, 32)}
	opened := h.open(1, []any{unknown, point(blocks[2]), []any{}}, 0, 10)
	if opened.header["type"] != "cs_opened" {
		t.Fatalf("got %v", opened.header)
	}
	samePoint(t, opened.header["point"], blocks[2])
	// The intersection is not the first point: the consumer moves back.
	back := h.expect("cs_roll_backward")
	samePoint(t, back.header["point"], blocks[2])
	h.forward(1, 2, blocks[3])
	h.forward(1, 3, blocks[4])
	h.quiet(150 * time.Millisecond)

	// From the origin the first block's parent is the genesis hash.
	opened = h.open(2, []any{[]any{}}, 0, 10)
	if opened.header["type"] != "cs_opened" || len(opened.header["point"].([]any)) != 0 {
		t.Fatalf("got %v", opened.header)
	}
	for i, block := range blocks {
		h.forward(2, uint64(i)+1, block)
	}

	notFound := h.open(3, []any{unknown}, 0, 10)
	if notFound.header["type"] != "cs_intersect_not_found" || asUint(t, notFound.header["stream"]) != 3 {
		t.Fatalf("got %v", notFound.header)
	}
}

// The first requested point is the consumer's position. The rollback that
// follows FindIntersect is delivered, at the next sequence number, exactly
// when the node intersected anywhere else.
func TestInitialRollbackIsDeliveredUnlessTheFirstPointIsTheIntersection(t *testing.T) {
	node, socket := startNode(t)
	blocks := extend(t, node, 6, 0)
	h := startSidecar(t, node, socket)
	h.hello()
	if f := h.open(1, []any{point(blocks[3]), point(blocks[1])}, 7, 10); f.header["type"] != "cs_opened" {
		t.Fatalf("got %v", f.header)
	}
	h.forward(1, 8, blocks[4])
	h.forward(1, 9, blocks[5])
	closeID := h.id()
	h.sendRaw(map[string]any{"type": "cs_close", "id": closeID, "stream": 1}, nil)
	h.expect("ok")

	// The consumer's block left the chain; the node intersects lower.
	node.Rollback(2)
	fork := extend(t, node, 2, 1)
	opened := h.open(2, []any{point(blocks[3]), point(blocks[1]), []any{}}, 7, 10)
	samePoint(t, opened.header["point"], blocks[1])
	back := h.expect("cs_roll_backward")
	if asUint(t, back.header["stream"]) != 2 || asUint(t, back.header["seq"]) != 8 {
		t.Fatalf("rollback %v", back.header)
	}
	samePoint(t, back.header["point"], blocks[1])
	h.forward(2, 9, fork[0])
	h.forward(2, 10, fork[1])
}

// A node whose first reply after FindIntersect is not the rollback to the
// intersection breaks the protocol; on the primary connection that ends
// the session.
func TestNonConformantFirstReplyIsAProtocolViolation(t *testing.T) {
	for name, first := range map[string]mocknode.FirstReply{
		"rollback elsewhere": mocknode.FirstReplyRollbackToOrigin,
		"roll forward":       mocknode.FirstReplyRollForward,
	} {
		t.Run(name, func(t *testing.T) {
			node, socket := startNode(t)
			blocks := extend(t, node, 4, 0)
			node.SetFirstReply(first)
			h := startSidecar(t, node, socket)
			h.hello()
			if f := h.open(1, []any{point(blocks[1])}, 0, 10); f.header["type"] != "cs_opened" {
				t.Fatalf("got %v", f.header)
			}
			if f := h.expect("fatal"); f.header["code"] != "protocol_violation" {
				t.Fatalf("got %v", f.header)
			}
			if status := <-h.status; status != exitFatal {
				t.Fatalf("status %d", status)
			}
			h.status <- 0
		})
	}
}

func TestCreditWindowBoundsRequestsAndFrames(t *testing.T) {
	node, socket := startNode(t)
	blocks := extend(t, node, 40, 0)
	h := startSidecar(t, node, socket)
	h.hello()
	if f := h.open(1, []any{[]any{}}, 100, 5); f.header["type"] != "cs_opened" {
		t.Fatalf("got %v", f.header)
	}
	for i := range 5 {
		h.forward(1, 101+uint64(i), blocks[i])
	}
	h.quiet(200 * time.Millisecond)
	// One request answered the intersection rollback, five the credit.
	if got := node.RequestNexts.Load(); got != 6 {
		t.Fatalf("node saw %d RequestNext with credit 5", got)
	}
	h.ack(1, 103)
	for i := 5; i < 8; i++ {
		h.forward(1, 101+uint64(i), blocks[i])
	}
	h.quiet(200 * time.Millisecond)
	if got := node.RequestNexts.Load(); got != 9 {
		t.Fatalf("node saw %d RequestNext after acking 3", got)
	}
	h.sendRaw(map[string]any{"type": "cs_window", "stream": 1, "window": 10}, nil)
	for i := 8; i < 13; i++ {
		h.forward(1, 101+uint64(i), blocks[i])
	}
	h.quiet(200 * time.Millisecond)
	// An ack outside the delivered range is a client fault.
	h.ack(1, 500)
	if f := h.expect("fatal"); f.header["code"] != "client_protocol_violation" {
		t.Fatalf("got %v", f.header)
	}
}

// A catch-up with a full credit window keeps RequestNext pipelined while the
// node holds agency across hundreds of replies, then reaches the tip with the
// window still open. gouroboros before v0.207 raced that send path against its
// state machine and failed the connection; a stream that queued its requests
// inline then blocked at the tip on the requests gouroboros holds after
// AwaitReply, leaving the last replies unread.
func TestSustainedPipelinedCatchUpDeliversEveryBlock(t *testing.T) {
	node, socket := startNode(t)
	blocks := extend(t, node, 600, 0)
	h := startSidecar(t, node, socket)
	h.hello()
	if f := h.open(1, []any{[]any{}}, 0, maxWindow); f.header["type"] != "cs_opened" {
		t.Fatalf("got %v", f.header)
	}
	for i, block := range blocks {
		h.forward(1, uint64(i)+1, block)
		h.ack(1, uint64(i)+1)
	}
}

func TestRollbackIsOrderedInTheSequence(t *testing.T) {
	node, socket := startNode(t)
	blocks := extend(t, node, 10, 0)
	h := startSidecar(t, node, socket)
	h.hello()
	h.open(1, []any{[]any{}}, 0, 50)
	for i, block := range blocks {
		h.forward(1, uint64(i)+1, block)
	}
	h.ack(1, 10)
	node.Rollback(7)
	fork := extend(t, node, 3, 1)
	back := h.expect("cs_roll_backward")
	if asUint(t, back.header["seq"]) != 11 {
		t.Fatalf("rollback seq %v", back.header["seq"])
	}
	samePoint(t, back.header["point"], blocks[6])
	for i, block := range fork {
		h.forward(1, 12+uint64(i), block)
	}
	if !bytes.Equal(fork[0].Prev, blocks[6].Hash) {
		t.Fatal("fork does not descend from the rollback point")
	}
}

func TestCloseAnswersOkAndNothingFollows(t *testing.T) {
	node, socket := startNode(t)
	blocks := extend(t, node, 30, 0)
	h := startSidecar(t, node, socket)
	h.hello()
	h.open(1, []any{[]any{}}, 0, 3)
	for i := range 3 {
		h.forward(1, uint64(i)+1, blocks[i])
	}
	closeID := h.id()
	h.sendRaw(map[string]any{"type": "cs_close", "id": closeID, "stream": 1}, nil)
	if f := h.expect("ok"); asUint(t, f.header["id"]) != closeID {
		t.Fatalf("got %v", f.header)
	}
	h.ack(1, 3)
	h.quiet(200 * time.Millisecond)
	// The primary chain-sync is lent again once drained.
	if f := h.open(2, []any{point(blocks[9])}, 0, 2); f.header["type"] != "cs_opened" {
		t.Fatalf("got %v", f.header)
	}
	h.forward(2, 1, blocks[10])
	h.forward(2, 2, blocks[11])
}

func TestConcurrentStreamsUseAuxiliaryConnections(t *testing.T) {
	node, socket := startNode(t)
	blocks := extend(t, node, 6, 0)
	h := startSidecar(t, node, socket)
	h.hello()
	h.open(1, []any{[]any{}}, 0, 1)
	h.forward(1, 1, blocks[0])
	if f := h.open(2, []any{point(blocks[3])}, 0, 5); f.header["type"] != "cs_opened" {
		t.Fatalf("got %v", f.header)
	}
	h.forward(2, 1, blocks[4])
	h.forward(2, 2, blocks[5])
	if got := node.Connections.Load(); got != 2 {
		t.Fatalf("expected one primary and one auxiliary connection, saw %d", got)
	}
	h.ack(1, 1)
	h.forward(1, 2, blocks[1])
}

func TestResumeAfterRestartHasNoGapAndNoDuplicate(t *testing.T) {
	node, socket := startNode(t)
	blocks := extend(t, node, 12, 0)
	h := startSidecar(t, node, socket)
	h.hello()
	h.open(1, []any{[]any{}}, 0, 4)
	for i := range 4 {
		h.forward(1, uint64(i)+1, blocks[i])
	}
	h.ack(1, 2)
	h.forward(1, 5, blocks[4])
	h.forward(1, 6, blocks[5])
	// The sidecar dies. The client resumes from its last received event.
	close(h.stop)
	<-h.status
	h.status <- 0
	r := startSidecar(t, node, socket)
	r.hello()
	opened := r.open(1, []any{point(blocks[5]), point(blocks[1]), []any{}}, 6, 4)
	samePoint(t, opened.header["point"], blocks[5])
	for i := 6; i < 10; i++ {
		r.forward(1, uint64(i)+1, blocks[i])
	}

	// Resuming after the last received block left the chain: the rollback
	// to the intersection is the next sequence number.
	close(r.stop)
	<-r.status
	r.status <- 0
	node.Rollback(8)
	fork := extend(t, node, 2, 1)
	q := startSidecar(t, node, socket)
	q.hello()
	opened = q.open(1, []any{point(blocks[9]), point(blocks[7]), []any{}}, 10, 4)
	samePoint(t, opened.header["point"], blocks[7])
	back := q.expect("cs_roll_backward")
	if asUint(t, back.header["seq"]) != 11 {
		t.Fatalf("rollback seq %v", back.header["seq"])
	}
	samePoint(t, back.header["point"], blocks[7])
	q.forward(1, 12, fork[0])
	q.forward(1, 13, fork[1])
}

func TestDeepDatumBlockPassesHeaderDecode(t *testing.T) {
	block, err := mocknode.MakeBlock(3, 30, mocknode.GenesisHash)
	if err != nil {
		t.Fatal(err)
	}
	got, number, prev, err := blockIdentity(mocknode.BlockType, block.Raw)
	if err != nil || number != 3 || got.slot != 30 || !bytes.Equal(got.hash, block.Hash) || !bytes.Equal(prev, mocknode.GenesisHash) {
		t.Fatalf("header identity mismatch: %v", err)
	}
	if _, _, _, err := blockIdentity(mocknode.BlockType, []byte{0x80}); err == nil {
		t.Fatal("an empty array was admitted as a block")
	}
}

func TestBlockBodyMustMatchItsHeader(t *testing.T) {
	block, err := mocknode.MakeBlock(3, 30, mocknode.GenesisHash)
	if err != nil {
		t.Fatal(err)
	}
	var parts []gcbor.RawMessage
	if _, err := gcbor.Decode(block.Raw, &parts); err != nil {
		t.Fatal(err)
	}
	// A different invalid-transactions set: same header, another body.
	tampered := append([]gcbor.RawMessage(nil), parts...)
	tampered[4], err = gcbor.Encode([]uint{0})
	if err != nil {
		t.Fatal(err)
	}
	raw, err := gcbor.Encode(tampered)
	if err != nil {
		t.Fatal(err)
	}
	if _, _, _, err := blockIdentity(mocknode.BlockType, raw); !errors.Is(err, errBlockBodyMismatch) {
		t.Fatalf("a block whose body differs from its header was admitted: %v", err)
	}
}

func TestLocalStateQueryReturnsRawAnswers(t *testing.T) {
	node, socket := startNode(t)
	blocks := extend(t, node, 3, 0)
	h := startSidecar(t, node, socket)
	h.hello()
	query := func(name string, extra map[string]any) received {
		header := map[string]any{"type": "lsq_query", "id": h.id(), "query": name}
		for k, v := range extra {
			header[k] = v
		}
		h.sendRaw(header, nil)
		return h.next()
	}
	if f := query("chain_point", nil); f.header["type"] != "error" || f.header["code"] != "not_acquired" {
		t.Fatalf("query before acquire: %v", f.header)
	}
	h.sendRaw(map[string]any{"type": "lsq_acquire", "id": h.id()}, nil)
	h.expect("ok")
	f := query("chain_point", nil)
	if f.header["type"] != "lsq_result" {
		t.Fatalf("got %v", f.header)
	}
	want, _ := cbor.Marshal([]any{blocks[2].Slot, blocks[2].Hash})
	if !bytes.Equal(f.payload, want) {
		t.Fatalf("chain point %x", f.payload)
	}
	credential := []any{0, bytes.Repeat([]byte{7}, 28)}
	f = query("filtered_delegations_and_rewards", map[string]any{"credentials": []any{credential}})
	echo, _ := cbor.Marshal([]any{10, cbor.Tag{Number: 258, Content: []any{credential}}})
	if f.header["type"] != "lsq_result" || !bytes.Equal(f.payload, echo) {
		t.Fatalf("era query answer is not the unwrapped raw answer: %v %x", f.header, f.payload)
	}
	node.SetEraMismatch(true)
	if f := query("protocol_params", nil); f.header["code"] != "era_mismatch" {
		t.Fatalf("got %v", f.header)
	}
	node.SetEraMismatch(false)
	h.sendRaw(map[string]any{"type": "lsq_acquire", "id": h.id(), "point": []any{uint64(5), bytes.Repeat([]byte{9}, 32)}}, nil)
	if f := h.next(); f.header["code"] != "acquire_point_not_on_chain" {
		t.Fatalf("got %v", f.header)
	}
	h.sendRaw(map[string]any{"type": "lsq_acquire", "id": h.id(), "point": point(blocks[1])}, nil)
	h.expect("ok")
	h.sendRaw(map[string]any{"type": "lsq_release", "id": h.id()}, nil)
	h.expect("ok")
	if f := query("bogus", nil); f.header["code"] != "not_acquired" && f.header["code"] != "unknown_query" {
		t.Fatalf("got %v", f.header)
	}
	acquired := node.Acquired()
	if len(acquired) != 2 || acquired[0] != "tip" {
		t.Fatalf("acquisitions %v", acquired)
	}
}

// A ledger request the node answers too slowly is refused with node_timeout
// at its deadline; the session and its chain-sync streams carry on, and the
// late reply is taken before the next request, never handed to it.
func TestSlowLedgerRequestsTimeOutAndTheSessionLivesOn(t *testing.T) {
	node, socket := startNode(t)
	blocks := extend(t, node, 3, 0)
	h := startSidecar(t, node, socket)
	h.helloWithDeadline(300)
	h.open(1, []any{[]any{}}, 0, 10)
	for i, block := range blocks {
		h.forward(1, uint64(i)+1, block)
	}
	h.sendRaw(map[string]any{"type": "lsq_acquire", "id": h.id()}, nil)
	h.expect("ok")

	node.SetLedgerDelay(time.Second)
	queryID := h.id()
	sent := time.Now()
	h.sendRaw(map[string]any{"type": "lsq_query", "id": queryID, "query": "system_start"}, nil)
	more := extend(t, node, 2, 0)
	timedOut, delivered := false, 0
	for !timedOut || delivered < len(more) {
		f := h.next()
		switch f.header["type"] {
		case "error":
			if asUint(t, f.header["id"]) != queryID || f.header["code"] != "node_timeout" {
				t.Fatalf("got %v", f.header)
			}
			if waited := time.Since(sent); waited >= time.Second {
				t.Fatalf("the refusal took %v, not the deadline", waited)
			}
			timedOut = true
		case "cs_roll_forward":
			if asUint(t, f.header["seq"]) != uint64(len(blocks)+delivered+1) {
				t.Fatalf("got %v", f.header)
			}
			samePoint(t, f.header["point"], more[delivered])
			delivered++
		default:
			t.Fatalf("got %v", f.header)
		}
	}

	// The late system_start answer is drained; this query gets its own.
	node.SetLedgerDelay(0)
	time.Sleep(time.Second)
	h.sendRaw(map[string]any{"type": "lsq_query", "id": h.id(), "query": "chain_point"}, nil)
	f := h.expect("lsq_result")
	want, _ := cbor.Marshal([]any{more[1].Slot, more[1].Hash})
	if !bytes.Equal(f.payload, want) {
		t.Fatalf("chain point %x is not the tip's", f.payload)
	}

	tx, _, err := mocknode.SampleTx()
	if err != nil {
		t.Fatal(err)
	}
	node.SetLedgerDelay(time.Second)
	h.sendRaw(map[string]any{"type": "submit", "id": h.id()}, tx)
	if f := h.expect("error"); f.header["code"] != "node_timeout" {
		t.Fatalf("got %v", f.header)
	}
	node.SetLedgerDelay(0)
	time.Sleep(time.Second)
	h.sendRaw(map[string]any{"type": "submit", "id": h.id()}, tx)
	h.expect("submit_accepted")
	if got := len(node.Mempool()); got != 2 {
		t.Fatalf("mempool holds %d transactions", got)
	}
}

func TestSubmitAndMonitor(t *testing.T) {
	node, socket := startNode(t)
	h := startSidecar(t, node, socket)
	h.hello()
	tx, txID, err := mocknode.SampleTx()
	if err != nil {
		t.Fatal(err)
	}
	hasTx := func() bool {
		h.sendRaw(map[string]any{"type": "monitor_has_tx", "id": h.id(), "txId": txID}, nil)
		return h.expect("monitor_has_tx_result").header["has"].(bool)
	}
	if hasTx() {
		t.Fatal("empty mempool reported the transaction")
	}
	reason := []byte{0x82, 0x01, 0x43, 0xaa, 0xbb, 0xcc}
	node.SetRejectReason(reason)
	h.sendRaw(map[string]any{"type": "submit", "id": h.id(), "era": 6}, tx)
	if f := h.expect("submit_rejected"); !bytes.Equal(f.payload, reason) {
		t.Fatalf("rejection bytes %x", f.payload)
	}
	node.SetRejectReason(nil)
	h.sendRaw(map[string]any{"type": "submit", "id": h.id()}, tx)
	h.expect("submit_accepted")
	if !hasTx() {
		t.Fatal("mempool does not report the accepted transaction")
	}
	h.sendRaw(map[string]any{"type": "monitor_sizes", "id": h.id()}, nil)
	if f := h.expect("monitor_sizes_result"); asUint(t, f.header["txCount"]) != 1 {
		t.Fatalf("got %v", f.header)
	}
	node.ClearMempool()
	if hasTx() {
		t.Fatal("has_tx answered from a stale snapshot")
	}
	// The node decodes only the hard-fork transaction id; the mock refuses
	// the bare hash as the node does.
	if eras := node.HasTxEras(); len(eras) != 3 || eras[0] != conwayEra || eras[2] != conwayEra {
		t.Fatalf("MsgHasTx era indices %v", eras)
	}
}

func TestNodeLossIsFatal(t *testing.T) {
	node, socket := startNode(t)
	extend(t, node, 2, 0)
	h := startSidecar(t, node, socket)
	h.hello()
	node.DropConnections()
	if f := h.expect("fatal"); f.header["code"] != "node_connection_lost" {
		t.Fatalf("got %v", f.header)
	}
	if status := <-h.status; status != exitFatal {
		t.Fatalf("status %d", status)
	}
	h.status <- 0
}

func TestMalformedFramesAreRefused(t *testing.T) {
	for name, frame := range map[string][]byte{
		"zero header":    {0, 0, 0, 0, 0, 0, 0, 0},
		"oversized":      {0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0},
		"not a map":      append([]byte{0, 0, 0, 1, 0, 0, 0, 0}, 0x01),
		"unknown type":   mustFrame(t, map[string]any{"type": "nope"}, nil),
		"unknown key":    mustFrame(t, map[string]any{"type": "cs_ack", "stream": 1, "seq": 0, "x": 1}, nil),
		"stray payload":  mustFrame(t, map[string]any{"type": "lsq_release", "id": 1}, []byte{1}),
		"stream reuse":   mustFrame(t, map[string]any{"type": "cs_open", "id": 1, "stream": 0, "points": []any{[]any{}}, "startSeq": 0, "window": 1}, nil),
		"invalid window": mustFrame(t, map[string]any{"type": "cs_open", "id": 1, "stream": 1, "points": []any{[]any{}}, "startSeq": 0, "window": 101}, nil),
	} {
		t.Run(name, func(t *testing.T) {
			node, socket := startNode(t)
			h := startSidecar(t, node, socket)
			h.hello()
			if _, err := h.in.Write(frame); err != nil {
				t.Fatal(err)
			}
			f := h.expect("fatal")
			if code := f.header["code"]; code != "malformed_frame" && code != "client_protocol_violation" {
				t.Fatalf("got %v", f.header)
			}
			if status := <-h.status; status != exitClientMisuse {
				t.Fatalf("status %d", status)
			}
			h.status <- 0
		})
	}
}

func mustFrame(t *testing.T, header any, payload []byte) []byte {
	t.Helper()
	encoded, err := cbor.Marshal(header)
	if err != nil {
		t.Fatal(err)
	}
	var lengths [8]byte
	binary.BigEndian.PutUint32(lengths[0:4], uint32(len(encoded)))
	binary.BigEndian.PutUint32(lengths[4:8], uint32(len(payload)))
	return append(append(lengths[:], encoded...), payload...)
}

// The bare-hash MsgHasTx that stock gouroboros sends is what cardano-node
// cannot decode; the mock refuses it the same way, so a regression to it
// fails here rather than only against a real node.
func TestMockRefusesBareHasTx(t *testing.T) {
	_, socket := startNode(t)
	node, err := dialNode(t.Context(), socket, testMagic, true, io.Discard)
	if err != nil {
		t.Fatal(err)
	}
	defer node.close()
	if err := node.monitor.send(localtxmonitor.NewMsgAcquire()); err != nil {
		t.Fatal(err)
	}
	if _, ok := (<-node.monitor.messages).(*localtxmonitor.MsgAcquired); !ok {
		t.Fatal("no MsgAcquired")
	}
	if err := node.monitor.send(localtxmonitor.NewMsgHasTx(make([]byte, 32))); err != nil {
		t.Fatal(err)
	}
	select {
	case <-node.dead:
	case msg := <-node.monitor.messages:
		t.Fatalf("the mock answered a bare-hash MsgHasTx: %T", msg)
	case <-time.After(10 * time.Second):
		t.Fatal("the mock kept the connection open")
	}
}
