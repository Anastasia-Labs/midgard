package main

import (
	"errors"
	"fmt"
	"sync"

	gcbor "github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
)

const (
	maxIntersectPoints = 256
	// maxIdleAux bounds the idle auxiliary connections kept for reuse.
	maxIdleAux = 4
)

// csLease is one chain-sync instance lent to one stream.
type csLease struct {
	node    *nodeConn
	primary bool
}

// csPool lends chain-sync instances. The primary connection's instance is
// lent first; a concurrently open stream gets an auxiliary connection, and an
// idle auxiliary connection is reused before a new one is dialled.
type csPool struct {
	mu          sync.Mutex
	session     *session
	primaryFree bool
	idle        []*nodeConn
	// aux holds every open auxiliary connection, lent or idle.
	aux    map[*nodeConn]struct{}
	closed bool
}

func (p *csPool) acquire() (*csLease, error) {
	p.mu.Lock()
	if p.primaryFree {
		p.primaryFree = false
		p.mu.Unlock()
		return &csLease{node: p.session.primary, primary: true}, nil
	}
	for len(p.idle) > 0 {
		node := p.idle[len(p.idle)-1]
		p.idle = p.idle[:len(p.idle)-1]
		select {
		case <-node.dead:
			continue
		default:
		}
		p.mu.Unlock()
		return &csLease{node: node}, nil
	}
	p.mu.Unlock()
	node, err := dialNode(p.session.ctx, p.session.socketPath, p.session.networkMagic, false, p.session.diagnostics)
	if err != nil {
		return nil, err
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.closed {
		go node.close()
		return nil, errors.New("the sidecar is stopping")
	}
	if p.aux == nil {
		p.aux = map[*nodeConn]struct{}{}
	}
	p.aux[node] = struct{}{}
	return &csLease{node: node}, nil
}

// release returns an Idle instance to the pool. A lease that is not Idle is
// never released: an auxiliary connection is closed instead.
func (p *csPool) release(lease *csLease) {
	p.mu.Lock()
	defer p.mu.Unlock()
	if lease.primary {
		p.primaryFree = true
		return
	}
	select {
	case <-lease.node.dead:
		delete(p.aux, lease.node)
		return
	default:
	}
	if p.closed || len(p.idle) >= maxIdleAux {
		delete(p.aux, lease.node)
		go lease.node.close()
		return
	}
	p.idle = append(p.idle, lease.node)
}

// discard closes an auxiliary connection that is not reusable.
func (p *csPool) discard(lease *csLease) {
	if lease.primary {
		return
	}
	p.mu.Lock()
	delete(p.aux, lease.node)
	p.mu.Unlock()
	lease.node.close()
}

func (p *csPool) closeAll() {
	p.mu.Lock()
	p.closed = true
	nodes := make([]*nodeConn, 0, len(p.aux))
	for node := range p.aux {
		nodes = append(nodes, node)
	}
	p.aux = nil
	p.idle = nil
	p.mu.Unlock()
	var wg sync.WaitGroup
	for _, node := range nodes {
		wg.Add(1)
		go func() {
			defer wg.Done()
			node.close()
		}()
	}
	wg.Wait()
}

// csStream is one credit-windowed chain-sync stream.
type csStream struct {
	id      uint64
	session *session
	wake    chan struct{}

	mu      sync.Mutex
	window  uint64
	acked   uint64
	lastSeq uint64
	closing bool
	closeID uint64
	failure *requestError
}

func (st *csStream) signal() {
	select {
	case st.wake <- struct{}{}:
	default:
	}
}

func (st *csStream) setWindow(window uint64) *requestError {
	if window == 0 || window > maxWindow {
		return refuse("invalid_window", "window must be within 1..%d", maxWindow)
	}
	st.mu.Lock()
	st.window = window
	st.mu.Unlock()
	st.signal()
	return nil
}

func (st *csStream) ack(seq uint64) *requestError {
	st.mu.Lock()
	defer st.mu.Unlock()
	if seq < st.acked || seq > st.lastSeq {
		return refuse("invalid_ack", "ack %d is outside the delivered range %d..%d", seq, st.acked, st.lastSeq)
	}
	st.acked = seq
	st.signal()
	return nil
}

func (st *csStream) requestClose(id uint64) {
	st.mu.Lock()
	if !st.closing {
		st.closing = true
		st.closeID = id
	}
	st.mu.Unlock()
	st.signal()
}

func tipOf(tip chainsync.Tip) wireTip {
	return wireTip{point: pointFromCommon(tip.Point), blockNo: tip.BlockNumber}
}

// blockIdentity decodes only the header of a raw block: its point, number
// and parent hash. The block body is neither decoded nor validated.
func blockIdentity(blockType uint, raw []byte) (wirePoint, uint64, []byte, error) {
	var parts []gcbor.RawMessage
	if _, err := gcbor.Decode(raw, &parts); err != nil || len(parts) == 0 {
		return wirePoint{}, 0, nil, fmt.Errorf("block is not a CBOR array: %v", err)
	}
	header, err := ledger.NewBlockHeaderFromCbor(blockType, parts[0])
	if err != nil {
		return wirePoint{}, 0, nil, fmt.Errorf("decode block header: %w", err)
	}
	hash := header.Hash()
	prev := header.PrevHash()
	var prevHash []byte
	if prev != (ledger.Blake2b256{}) {
		prevHash = append([]byte(nil), prev.Bytes()...)
	}
	return wirePoint{slot: header.SlotNumber(), hash: append([]byte(nil), hash.Bytes()...)}, header.BlockNumber(), prevHash, nil
}

type streamFailure struct {
	code  string
	cause error
	// fatal marks a primary-connection fault: the sidecar ends.
	fatal bool
}

// run owns the stream from FindIntersect to close. Every frame for the
// stream is written from here, so frames are in sequence order.
func (st *csStream) run(open csOpenHeader) {
	pool := st.session.pool
	lease, err := pool.acquire()
	if err != nil {
		st.session.dropStream(st.id)
		st.session.answerError(open.ID, refuse("node_unavailable", "dial chain-sync connection: %v", err))
		return
	}
	cs := lease.node.chainSync
	points := make([]pcommon.Point, len(open.Points))
	for i, point := range open.Points {
		points[i] = point.common()
	}
	failure := func(f streamFailure) {
		st.session.dropStream(st.id)
		if f.fatal || lease.primary {
			st.session.fatal(f.code, f.cause)
			return
		}
		pool.discard(lease)
		_ = st.session.out.write(csFailedHeader{Type: "cs_failed", Stream: st.id, Code: f.code, Message: f.cause.Error()}, nil)
	}
	if err := cs.send(chainsync.NewMsgFindIntersect(points)); err != nil {
		st.session.dropStream(st.id)
		if lease.primary {
			st.session.fatal("node_connection_lost", err)
			return
		}
		pool.discard(lease)
		st.session.answerError(open.ID, refuse("node_unavailable", "find intersect: %v", err))
		return
	}
	var intersect wirePoint
	select {
	case msg := <-cs.messages:
		switch reply := msg.(type) {
		case *chainsync.MsgIntersectFound:
			intersect = pointFromCommon(reply.Point)
			_ = st.session.out.write(csOpenedHeader{Type: "cs_opened", ID: open.ID, Stream: st.id, Point: intersect, Tip: tipOf(reply.Tip)}, nil)
		case *chainsync.MsgIntersectNotFound:
			st.session.dropStream(st.id)
			pool.release(lease)
			_ = st.session.out.write(csIntersectNotFoundHeader{Type: "cs_intersect_not_found", ID: open.ID, Stream: st.id, Tip: tipOf(reply.Tip)}, nil)
			return
		default:
			// The open is answered once: a primary fault ends the sidecar,
			// an auxiliary one answers the open with the error.
			st.session.dropStream(st.id)
			cause := fmt.Errorf("unexpected reply to FindIntersect: message type %d", msg.Type())
			if lease.primary {
				st.session.fatal("protocol_violation", cause)
				return
			}
			pool.discard(lease)
			st.session.answerError(open.ID, refuse("protocol_violation", "%v", cause))
			return
		}
	case <-lease.node.dead:
		st.session.dropStream(st.id)
		if lease.primary {
			st.session.fatal("node_connection_lost", lease.node.deathCause())
			return
		}
		pool.discard(lease)
		st.session.answerError(open.ID, refuse("node_unavailable", "%v", lease.node.deathCause()))
		return
	}

	// RequestNext is queued from its own goroutine. Once the node answers
	// AwaitReply at the tip, gouroboros holds further requests until agency
	// returns and its bounded send queue then blocks the sender; the stream
	// must still read replies, acknowledgements and its close meanwhile.
	// inFlight never exceeds maxWindow, so a request token never blocks.
	requests := make(chan struct{}, maxWindow)
	sendFailed := make(chan error, 1)
	stopSender := make(chan struct{})
	defer close(stopSender)
	go func() {
		for {
			select {
			case <-requests:
			case <-stopSender:
				return
			}
			if err := cs.send(chainsync.NewMsgRequestNext()); err != nil {
				sendFailed <- err
				return
			}
		}
	}()

	inFlight := uint64(0)
	awaitingFirst := true
	answeredClose := false
	for {
		st.mu.Lock()
		closing := st.closing
		closeID := st.closeID
		can := !closing && inFlight+(st.lastSeq-st.acked) < st.window
		st.mu.Unlock()
		if closing && !answeredClose {
			// No frame for this stream follows its close answer.
			st.session.dropStream(st.id)
			_ = st.session.out.write(okHeader{Type: "ok", ID: closeID}, nil)
			answeredClose = true
		}
		if closing && inFlight == 0 {
			pool.release(lease)
			return
		}
		if closing && !lease.primary {
			pool.discard(lease)
			return
		}
		if can {
			requests <- struct{}{}
			inFlight++
			continue
		}
		select {
		case <-st.wake:
			continue
		case err := <-sendFailed:
			failure(streamFailure{code: "node_connection_lost", cause: err})
			return
		case <-lease.node.dead:
			if closing && !lease.primary {
				return
			}
			failure(streamFailure{code: "node_connection_lost", cause: lease.node.deathCause()})
			return
		case msg := <-cs.messages:
			switch reply := msg.(type) {
			case *chainsync.MsgAwaitReply:
				continue
			case *chainsync.MsgRollBackward:
				inFlight--
				if closing {
					continue
				}
				point := pointFromCommon(reply.Point)
				if awaitingFirst {
					awaitingFirst = false
					if !point.equal(intersect) {
						failure(streamFailure{code: "protocol_violation", cause: errors.New("first reply after FindIntersect is not a rollback to the intersection")})
						return
					}
					// The first requested point is the consumer's position.
					// Intersecting there needs no rollback; intersecting
					// anywhere else moves the consumer back to the
					// intersection, so the rollback is delivered.
					if open.Points[0].equal(intersect) {
						continue
					}
				}
				seq := st.nextSeq()
				if err := st.session.out.write(csRollBackwardHeader{Type: "cs_roll_backward", Stream: st.id, Seq: seq, Point: point, Tip: tipOf(reply.Tip)}, nil); err != nil {
					st.session.fatal("output_closed", err)
					return
				}
			case *chainsync.MsgRollForwardNtC:
				inFlight--
				if closing {
					continue
				}
				if awaitingFirst {
					failure(streamFailure{code: "protocol_violation", cause: errors.New("first reply after FindIntersect is not a rollback to the intersection")})
					return
				}
				raw := reply.BlockCbor()
				if len(raw) == 0 || len(raw) > maxPayloadBytes {
					failure(streamFailure{code: "block_bounds", cause: errors.New("block size is outside the frame bound")})
					return
				}
				point, blockNo, prevHash, err := blockIdentity(reply.BlockType(), raw)
				if err != nil {
					failure(streamFailure{code: "block_header_undecodable", cause: err})
					return
				}
				seq := st.nextSeq()
				if err := st.session.out.write(csRollForwardHeader{
					Type: "cs_roll_forward", Stream: st.id, Seq: seq, Point: point, BlockNo: blockNo,
					BlockType: uint64(reply.BlockType()), PrevHash: prevHash, Tip: tipOf(reply.Tip),
				}, raw); err != nil {
					st.session.fatal("output_closed", err)
					return
				}
			default:
				failure(streamFailure{code: "protocol_violation", cause: fmt.Errorf("unexpected chain-sync message type %d", msg.Type())})
				return
			}
		}
	}
}

func (st *csStream) nextSeq() uint64 {
	st.mu.Lock()
	defer st.mu.Unlock()
	st.lastSeq++
	return st.lastSeq
}
