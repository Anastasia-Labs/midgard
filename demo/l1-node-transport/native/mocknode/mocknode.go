// Package mocknode is a deterministic in-process N2C peer for the transport
// sidecar's tests. It serves ChainSync over a scripted chain, a canned
// LocalStateQuery, LocalTxSubmission with scriptable rejection bytes and a
// LocalTxMonitor mempool, on a unix socket, using gouroboros in server mode.
// It makes no ledger-validity claim about the blocks it serves.
package mocknode

import (
	"bytes"
	_ "embed"
	"encoding/hex"
	"errors"
	"fmt"
	"net"
	"strings"
	"sync"
	"sync/atomic"

	ouroboros "github.com/blinklabs-io/gouroboros"
	gcbor "github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/protocol"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
	"github.com/blinklabs-io/gouroboros/protocol/localstatequery"
	"github.com/blinklabs-io/gouroboros/protocol/localtxmonitor"
	"github.com/blinklabs-io/gouroboros/protocol/localtxsubmission"
	"golang.org/x/crypto/blake2b"
)

//go:embed conway-block.hex
var conwayBlockHex string

// BlockType is the N2C block type of the served (Conway) blocks.
const BlockType = 7

// ConwayEra is the hard-fork era index the mock reports.
const ConwayEra = 6

// GenesisHash is the parent hash of block 1.
var GenesisHash = bytes.Repeat([]byte{0xee}, 32)

// Block is one served block.
type Block struct {
	Raw    []byte
	Number uint64
	Slot   uint64
	Hash   []byte
	Prev   []byte
}

// Point is the block's chain point.
func (b Block) Point() pcommon.Point { return pcommon.NewPoint(b.Slot, b.Hash) }

type template struct {
	block      []gcbor.RawMessage
	header     []gcbor.RawMessage
	headerBody []gcbor.RawMessage
}

func loadTemplate() (template, error) {
	raw, err := hex.DecodeString(strings.TrimSpace(conwayBlockHex))
	if err != nil {
		return template{}, err
	}
	var t template
	if _, err := gcbor.Decode(raw, &t.block); err != nil {
		return template{}, err
	}
	if _, err := gcbor.Decode(t.block[0], &t.header); err != nil {
		return template{}, err
	}
	if _, err := gcbor.Decode(t.header[0], &t.headerBody); err != nil {
		return template{}, err
	}
	return t, nil
}

func encode(value any) gcbor.RawMessage {
	raw, err := gcbor.Encode(value)
	if err != nil {
		panic(err)
	}
	return raw
}

// MakeBlock builds a Conway block with the given number, slot and parent by
// rewriting the header of the embedded real block.
func MakeBlock(number, slot uint64, prev []byte) (Block, error) {
	t, err := loadTemplate()
	if err != nil {
		return Block{}, err
	}
	body := append([]gcbor.RawMessage(nil), t.headerBody...)
	body[0] = encode(number)
	body[1] = encode(slot)
	body[2] = encode(prev)
	header := append([]gcbor.RawMessage(nil), t.header...)
	header[0] = encode(body)
	block := append([]gcbor.RawMessage(nil), t.block...)
	block[0] = encode(header)
	raw := encode(block)
	hash := blake2b.Sum256(block[0])
	return Block{Raw: raw, Number: number, Slot: slot, Hash: hash[:], Prev: append([]byte(nil), prev...)}, nil
}

// SampleTx returns a Conway transaction taken from the embedded block, and
// its id.
func SampleTx() ([]byte, []byte, error) {
	t, err := loadTemplate()
	if err != nil {
		return nil, nil, err
	}
	var bodies, witnesses []gcbor.RawMessage
	if _, err := gcbor.Decode(t.block[1], &bodies); err != nil {
		return nil, nil, err
	}
	if _, err := gcbor.Decode(t.block[2], &witnesses); err != nil {
		return nil, nil, err
	}
	if len(bodies) == 0 {
		return nil, nil, errors.New("template block has no transactions")
	}
	tx := encode([]any{bodies[0], witnesses[0], true, nil})
	id := blake2b.Sum256(bodies[0])
	return tx, id[:], nil
}

// Node is the mock node.
type Node struct {
	listener net.Listener
	magic    uint32

	mu       sync.Mutex
	chain    []Block
	changed  chan struct{}
	mempool  [][]byte
	reject   []byte
	answers  map[uint64][]byte
	mismatch bool
	acquired []string
	hasTxEra []uint64
	conns    map[*ouroboros.Connection]struct{}
	closed   bool

	// RequestNexts counts RequestNext messages handled, over all connections.
	RequestNexts atomic.Int64
	// Connections counts accepted connections.
	Connections atomic.Int64
}

// Start listens on socketPath and serves every accepted connection.
func Start(socketPath string, magic uint32) (*Node, error) {
	listener, err := net.Listen("unix", socketPath)
	if err != nil {
		return nil, err
	}
	n := &Node{
		listener: listener, magic: magic, changed: make(chan struct{}),
		answers: map[uint64][]byte{}, conns: map[*ouroboros.Connection]struct{}{},
	}
	go n.accept()
	return n, nil
}

func (n *Node) accept() {
	for {
		conn, err := n.listener.Accept()
		if err != nil {
			return
		}
		n.Connections.Add(1)
		go n.serve(conn)
	}
}

// Close stops listening and drops every connection.
func (n *Node) Close() {
	n.mu.Lock()
	n.closed = true
	n.mu.Unlock()
	_ = n.listener.Close()
	n.DropConnections()
}

// DropConnections closes every open connection, as a node restart would.
func (n *Node) DropConnections() {
	n.mu.Lock()
	conns := n.conns
	n.conns = map[*ouroboros.Connection]struct{}{}
	n.mu.Unlock()
	for conn := range conns {
		_ = conn.Close()
	}
}

func (n *Node) notifyLocked() {
	close(n.changed)
	n.changed = make(chan struct{})
}

// Extend appends count blocks. branch distinguishes fork blocks of one
// height: block h of branch b sits at slot 10*h + b.
func (n *Node) Extend(count int, branch uint64) ([]Block, error) {
	n.mu.Lock()
	defer n.mu.Unlock()
	added := make([]Block, 0, count)
	for range count {
		prev := GenesisHash
		number := uint64(len(n.chain)) + 1
		if len(n.chain) > 0 {
			prev = n.chain[len(n.chain)-1].Hash
		}
		block, err := MakeBlock(number, 10*number+branch, prev)
		if err != nil {
			return nil, err
		}
		n.chain = append(n.chain, block)
		added = append(added, block)
	}
	n.notifyLocked()
	return added, nil
}

// Rollback truncates the chain to height blocks.
func (n *Node) Rollback(height uint64) {
	n.mu.Lock()
	defer n.mu.Unlock()
	if height < uint64(len(n.chain)) {
		n.chain = n.chain[:height]
		n.notifyLocked()
	}
}

// Chain returns a copy of the current chain.
func (n *Node) Chain() []Block {
	n.mu.Lock()
	defer n.mu.Unlock()
	return append([]Block(nil), n.chain...)
}

// SetRejectReason makes submissions fail with these raw bytes (nil accepts).
func (n *Node) SetRejectReason(reason []byte) {
	n.mu.Lock()
	n.reject = append([]byte(nil), reason...)
	if reason == nil {
		n.reject = nil
	}
	n.mu.Unlock()
}

// SetShelleyAnswer sets the raw answer of a Shelley-era query tag. Without
// one, a Shelley query is answered with its own raw [tag, params...].
func (n *Node) SetShelleyAnswer(tag uint64, raw []byte) {
	n.mu.Lock()
	n.answers[tag] = append([]byte(nil), raw...)
	n.mu.Unlock()
}

// SetEraMismatch makes every Shelley-era query answer an era mismatch.
func (n *Node) SetEraMismatch(mismatch bool) {
	n.mu.Lock()
	n.mismatch = mismatch
	n.mu.Unlock()
}

// Acquired lists the LSQ acquisitions seen, as "tip" or "slot:hash".
func (n *Node) Acquired() []string {
	n.mu.Lock()
	defer n.mu.Unlock()
	return append([]string(nil), n.acquired...)
}

func (n *Node) recordHasTxEra(era uint64) {
	n.mu.Lock()
	n.hasTxEra = append(n.hasTxEra, era)
	n.mu.Unlock()
}

// HasTxEras lists the era index of every MsgHasTx received.
func (n *Node) HasTxEras() []uint64 {
	n.mu.Lock()
	defer n.mu.Unlock()
	return append([]uint64{}, n.hasTxEra...)
}

// Mempool lists the raw transactions currently in the mempool.
func (n *Node) Mempool() [][]byte {
	n.mu.Lock()
	defer n.mu.Unlock()
	return append([][]byte(nil), n.mempool...)
}

// ClearMempool empties the mempool.
func (n *Node) ClearMempool() {
	n.mu.Lock()
	n.mempool = nil
	n.mu.Unlock()
}

func (n *Node) tipLocked() chainsync.Tip {
	if len(n.chain) == 0 {
		return chainsync.Tip{Point: pcommon.NewPointOrigin()}
	}
	last := n.chain[len(n.chain)-1]
	return chainsync.Tip{Point: last.Point(), BlockNumber: last.Number}
}

// heightOfLocked finds point on the chain: 0 for the origin.
func (n *Node) heightOfLocked(point pcommon.Point) (uint64, bool) {
	if len(point.Hash) == 0 {
		return 0, true
	}
	for i, block := range n.chain {
		if block.Slot == point.Slot && bytes.Equal(block.Hash, point.Hash) {
			return uint64(i) + 1, true
		}
	}
	return 0, false
}

// follower is one connection's chain-sync read pointer.
type follower struct {
	node *Node
	// path holds the points delivered on this connection, from the
	// intersection; path[0] is the intersection.
	path            []pcommon.Point
	pendingRollback bool
	requests        chan struct{}
	responder       sync.Once
}

// respond answers each received RequestNext in order.
func (f *follower) respond(server *chainsync.Server, done <-chan struct{}) {
	n := f.node
	for {
		select {
		case <-f.requests:
		case <-done:
			return
		}
		awaited := false
		for {
			n.mu.Lock()
			changed := n.changed
			n.mu.Unlock()
			answered, err := f.step(server)
			if err != nil {
				return
			}
			if answered {
				break
			}
			if !awaited {
				if err := server.AwaitReply(); err != nil {
					return
				}
				awaited = true
			}
			select {
			case <-changed:
			case <-done:
				return
			}
		}
	}
}

type rejectReason struct{ raw []byte }

func (r rejectReason) Error() string                { return "mock rejection" }
func (r rejectReason) MarshalCBOR() ([]byte, error) { return r.raw, nil }

// step answers one RequestNext if the chain allows it now. It returns false
// when the reader is at the tip.
func (f *follower) step(server *chainsync.Server) (bool, error) {
	n := f.node
	n.mu.Lock()
	tip := n.tipLocked()
	if f.pendingRollback {
		f.pendingRollback = false
		point := f.path[0]
		n.mu.Unlock()
		return true, server.RollBackward(point, tip)
	}
	// A delivered point that left the chain is rolled back to the highest
	// delivered point still on it, or the origin.
	last := len(f.path) - 1
	keep := -1
	for i := last; i >= 0; i-- {
		if _, ok := n.heightOfLocked(f.path[i]); ok {
			keep = i
			break
		}
	}
	if keep != last {
		var point pcommon.Point
		if keep < 0 {
			point = pcommon.NewPointOrigin()
			f.path = []pcommon.Point{point}
		} else {
			point = f.path[keep]
			f.path = f.path[:keep+1]
		}
		n.mu.Unlock()
		return true, server.RollBackward(point, tip)
	}
	height, _ := n.heightOfLocked(f.path[last])
	if height < uint64(len(n.chain)) {
		block := n.chain[height]
		f.path = append(f.path, block.Point())
		n.mu.Unlock()
		return true, server.RollForward(BlockType, block.Raw, tip)
	}
	n.mu.Unlock()
	return false, nil
}

func (n *Node) serve(socket net.Conn) {
	f := &follower{node: n, requests: make(chan struct{}, chainsync.MaxPipelineLimit+1)}
	done := make(chan struct{})
	errorChan := make(chan error, 8)
	conn, err := ouroboros.New(
		ouroboros.WithConnection(&hfcConn{Conn: socket, eras: n.recordHasTxEra}),
		ouroboros.WithServer(true),
		ouroboros.WithNodeToNode(false),
		ouroboros.WithNetworkMagic(n.magic),
		ouroboros.WithErrorChan(errorChan),
		ouroboros.WithChainSyncConfig(chainsync.NewConfig(
			chainsync.WithFindIntersectFunc(func(_ chainsync.CallbackContext, points []pcommon.Point) (pcommon.Point, chainsync.Tip, error) {
				n.mu.Lock()
				defer n.mu.Unlock()
				tip := n.tipLocked()
				for _, point := range points {
					if _, ok := n.heightOfLocked(point); ok {
						f.path = []pcommon.Point{point}
						f.pendingRollback = true
						return point, tip, nil
					}
				}
				return pcommon.Point{}, tip, chainsync.ErrIntersectNotFound
			}),
			// Requests may arrive pipelined; one responder answers them in
			// order, as the node does, waiting at the tip for the chain to
			// change after a single AwaitReply per request.
			chainsync.WithRequestNextFunc(func(ctx chainsync.CallbackContext) error {
				n.RequestNexts.Add(1)
				f.responder.Do(func() { go f.respond(ctx.Server, done) })
				select {
				case f.requests <- struct{}{}:
					return nil
				case <-done:
					return protocol.ErrProtocolShuttingDown
				}
			}),
			chainsync.WithRecvQueueSize(chainsync.MaxRecvQueueSize),
		)),
		ouroboros.WithLocalStateQueryConfig(localstatequery.NewConfig(
			localstatequery.WithAcquireFunc(func(_ localstatequery.CallbackContext, target localstatequery.AcquireTarget, _ bool) error {
				n.mu.Lock()
				defer n.mu.Unlock()
				switch t := target.(type) {
				case localstatequery.AcquireSpecificPoint:
					if _, ok := n.heightOfLocked(t.Point); !ok {
						return localstatequery.ErrAcquireFailurePointNotOnChain
					}
					n.acquired = append(n.acquired, fmt.Sprintf("%d:%x", t.Point.Slot, t.Point.Hash))
				default:
					n.acquired = append(n.acquired, "tip")
				}
				return nil
			}),
			localstatequery.WithQueryFunc(func(_ localstatequery.CallbackContext, query localstatequery.QueryWrapper) (any, error) {
				return n.answer(query.Cbor())
			}),
			localstatequery.WithReleaseFunc(func(localstatequery.CallbackContext) error { return nil }),
		)),
		ouroboros.WithLocalTxSubmissionConfig(localtxsubmission.NewConfig(
			localtxsubmission.WithSubmitTxFunc(func(_ localtxsubmission.CallbackContext, tx localtxsubmission.MsgSubmitTxTransaction) error {
				n.mu.Lock()
				defer n.mu.Unlock()
				if n.reject != nil {
					return rejectReason{raw: n.reject}
				}
				raw, ok := tx.Raw.Content.([]byte)
				if !ok {
					return errors.New("transaction is not wrapped bytes")
				}
				n.mempool = append(n.mempool, append([]byte(nil), raw...))
				return nil
			}),
		)),
		ouroboros.WithLocalTxMonitorConfig(localtxmonitor.NewConfig(
			localtxmonitor.WithGetMempoolFunc(func(localtxmonitor.CallbackContext) (uint64, uint32, []localtxmonitor.TxAndEraId, error) {
				n.mu.Lock()
				defer n.mu.Unlock()
				txs := make([]localtxmonitor.TxAndEraId, 0, len(n.mempool))
				for _, tx := range n.mempool {
					txs = append(txs, localtxmonitor.TxAndEraId{EraId: ConwayEra, Tx: tx})
				}
				return n.tipLocked().Point.Slot, 1 << 20, txs, nil
			}),
		)),
	)
	if err != nil {
		_ = socket.Close()
		return
	}
	n.mu.Lock()
	if n.closed {
		n.mu.Unlock()
		_ = conn.Close()
		return
	}
	n.conns[conn] = struct{}{}
	n.mu.Unlock()
	go func() {
		select {
		case <-errorChan:
		case <-conn.ChainSync().Server.ProtocolInstance().DoneChan():
		}
		close(done)
		n.mu.Lock()
		delete(n.conns, conn)
		n.mu.Unlock()
		_ = conn.Close()
	}()
}

// answer serves one LSQ query from its raw CBOR.
func (n *Node) answer(raw []byte) (any, error) {
	var query []gcbor.RawMessage
	if _, err := gcbor.Decode(raw, &query); err != nil || len(query) == 0 {
		return nil, errors.New("query is not an array")
	}
	var tag uint64
	if _, err := gcbor.Decode(query[0], &tag); err != nil {
		return nil, err
	}
	n.mu.Lock()
	defer n.mu.Unlock()
	tip := n.tipLocked()
	switch tag {
	case 1: // SystemStart
		return []any{2022, 100, 0}, nil
	case 2: // ChainBlockNo
		if len(n.chain) == 0 {
			return []any{0}, nil
		}
		return []any{1, tip.BlockNumber}, nil
	case 3: // ChainPoint
		if len(n.chain) == 0 {
			return []any{}, nil
		}
		return []any{tip.Point.Slot, tip.Point.Hash}, nil
	case 0:
		var block []gcbor.RawMessage
		if _, err := gcbor.Decode(query[1], &block); err != nil || len(block) != 2 {
			return nil, errors.New("block query is malformed")
		}
		var kind uint64
		if _, err := gcbor.Decode(block[0], &kind); err != nil {
			return nil, err
		}
		switch kind {
		case 2: // hard-fork query
			var hf []uint64
			if _, err := gcbor.Decode(block[1], &hf); err != nil || len(hf) != 1 {
				return nil, errors.New("hard-fork query is malformed")
			}
			if hf[0] == 1 {
				return ConwayEra, nil
			}
			return gcbor.RawMessage(encode([]any{"era-history"})), nil
		case 0: // Shelley-based era query: [era, [tag, params...]]
			var shelley []gcbor.RawMessage
			if _, err := gcbor.Decode(block[1], &shelley); err != nil || len(shelley) != 2 {
				return nil, errors.New("era query is malformed")
			}
			if n.mismatch {
				return []any{ConwayEra, ConwayEra + 1}, nil
			}
			var inner []gcbor.RawMessage
			if _, err := gcbor.Decode(shelley[1], &inner); err != nil || len(inner) == 0 {
				return nil, errors.New("era query body is malformed")
			}
			var innerTag uint64
			if _, err := gcbor.Decode(inner[0], &innerTag); err != nil {
				return nil, err
			}
			if answer, ok := n.answers[innerTag]; ok {
				return []any{gcbor.RawMessage(answer)}, nil
			}
			return []any{gcbor.RawMessage(append([]byte(nil), shelley[1]...))}, nil
		}
	}
	return nil, fmt.Errorf("unsupported query %x", raw)
}
