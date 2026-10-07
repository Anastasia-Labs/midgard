package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"net"
	"sync"
	"time"

	ouroboros "github.com/blinklabs-io/gouroboros"
	"github.com/blinklabs-io/gouroboros/protocol"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	"github.com/blinklabs-io/gouroboros/protocol/localstatequery"
	"github.com/blinklabs-io/gouroboros/protocol/localtxmonitor"
	"github.com/blinklabs-io/gouroboros/protocol/localtxsubmission"
)

const (
	nodeDialTimeout = 10 * time.Second
	// maxWindow bounds the credit window. The receive queue of the chain-sync
	// instance is sized to it, so a full window never blocks the muxer.
	maxWindow = 100
	// closeBound bounds how long releasing one node connection may take.
	closeBound = 2 * time.Second
)

// nodeConn is one N2C connection to the local node. The primary connection
// carries LocalStateQuery, LocalTxSubmission, LocalTxMonitor and one
// chain-sync instance; an auxiliary connection carries one chain-sync
// instance only (see README.md, "Connections").
type nodeConn struct {
	conn      *ouroboros.Connection
	socket    net.Conn
	errors    chan error
	dead      chan struct{}
	deathOnce sync.Once
	cause     error
	causeMu   sync.Mutex

	chainSync *rawProtocol
	lsq       *rawProtocol
	submit    *rawProtocol
	monitor   *rawProtocol
}

// rawProtocol is a mini-protocol client whose received messages are handed,
// in order, to one owning goroutine.
type rawProtocol struct {
	proto    *protocol.Protocol
	messages chan protocol.Message
}

func (p *rawProtocol) send(msg protocol.Message) error {
	return p.proto.SendMessage(msg)
}

func (n *nodeConn) die(cause error) {
	n.deathOnce.Do(func() {
		n.causeMu.Lock()
		n.cause = cause
		n.causeMu.Unlock()
		close(n.dead)
	})
}

func (n *nodeConn) deathCause() error {
	n.causeMu.Lock()
	defer n.causeMu.Unlock()
	if n.cause == nil {
		return errors.New("node connection closed")
	}
	return n.cause
}

// close stops the connection within closeBound; a wedged Close is abandoned.
func (n *nodeConn) close() {
	n.die(errors.New("node connection closed by the sidecar"))
	_ = n.socket.Close()
	done := make(chan struct{})
	go func() {
		_ = n.conn.Close()
		close(done)
	}()
	select {
	case <-done:
	case <-time.After(closeBound):
	}
}

func newRawProtocol(
	conn *ouroboros.Connection,
	errorChan chan error,
	logger *slog.Logger,
	name string,
	id uint16,
	stateMap protocol.StateMap,
	initialState protocol.State,
	fromCbor protocol.MessageFromCborFunc,
	queue int,
) *rawProtocol {
	raw := &rawProtocol{messages: make(chan protocol.Message, queue)}
	raw.proto = protocol.New(protocol.ProtocolConfig{
		Name:       name,
		ProtocolId: id,
		ErrorChan:  errorChan,
		Muxer:      conn.Muxer(),
		Logger:     logger,
		Mode:       protocol.ProtocolModeNodeToClient,
		Role:       protocol.ProtocolRoleClient,
		MessageHandlerFunc: func(msg protocol.Message) error {
			select {
			case raw.messages <- msg:
				return nil
			case <-raw.proto.DoneChan():
				return protocol.ErrProtocolShuttingDown
			}
		},
		MessageFromCborFunc: fromCbor,
		StateMap:            stateMap,
		InitialState:        initialState,
		RecvQueueSize:       queue,
		MaxReadBufferSize:   maxPayloadBytes,
	})
	return raw
}

func idleState(stateMap protocol.StateMap) protocol.State {
	for state := range stateMap {
		if state.Id == 1 {
			return state
		}
	}
	panic("state map has no Idle state")
}

// dialError names why a node connection could not be opened:
// node_unreachable (the socket) or node_handshake_failed (the N2C handshake).
type dialError struct {
	code  string
	cause error
}

func (e *dialError) Error() string { return e.cause.Error() }
func (e *dialError) Unwrap() error { return e.cause }

// dialNode opens one N2C connection. primary selects the full protocol set.
func dialNode(ctx context.Context, socketPath string, networkMagic uint32, primary bool, diagnostics io.Writer) (*nodeConn, error) {
	dialer := net.Dialer{Timeout: nodeDialTimeout}
	socket, err := dialer.DialContext(ctx, "unix", socketPath)
	if err != nil {
		return nil, &dialError{code: "node_unreachable", cause: err}
	}
	errorChan := make(chan error, 16)
	logger := slog.New(slog.NewJSONHandler(diagnostics, &slog.HandlerOptions{Level: slog.LevelWarn}))
	conn, err := ouroboros.New(
		ouroboros.WithConnection(&streamSegmentConn{Conn: socket}),
		ouroboros.WithNetworkMagic(networkMagic),
		ouroboros.WithNodeToNode(false),
		ouroboros.WithErrorChan(errorChan),
		ouroboros.WithLogger(logger),
		ouroboros.WithDelayProtocolStart(true),
	)
	if err != nil {
		_ = socket.Close()
		return nil, &dialError{code: "node_handshake_failed", cause: fmt.Errorf("node handshake: %w", err)}
	}
	node := &nodeConn{conn: conn, socket: socket, errors: errorChan, dead: make(chan struct{})}
	// The stock N2C state map lets RequestNext be written while the node
	// holds agency in CanAwait, so the credited requests stay pipelined on
	// the wire; each one's transition is applied when agency returns.
	csMap := chainsync.StateMapNtC.Copy()
	node.chainSync = newRawProtocol(conn, errorChan, logger, chainsync.ProtocolName, chainsync.ProtocolIdNtC,
		csMap, idleState(csMap), chainsync.NewMsgFromCborNtC, maxWindow+8)
	node.chainSync.proto.Start()
	if primary {
		node.lsq = newRawProtocol(conn, errorChan, logger, localstatequery.ProtocolName, localstatequery.ProtocolId,
			localstatequery.StateMap.Copy(), idleState(localstatequery.StateMap), localstatequery.NewMsgFromCbor, 4)
		node.lsq.proto.Start()
		node.submit = newRawProtocol(conn, errorChan, logger, localtxsubmission.ProtocolName, localtxsubmission.ProtocolId,
			localtxsubmission.StateMap.Copy(), idleState(localtxsubmission.StateMap), localtxsubmission.NewMsgFromCbor, 4)
		node.submit.proto.Start()
		if conn.LocalTxMonitor() != nil {
			node.monitor = newRawProtocol(conn, errorChan, logger, localtxmonitor.ProtocolName, localtxmonitor.ProtocolId,
				localtxmonitor.StateMap.Copy(), idleState(localtxmonitor.StateMap), localtxmonitor.NewMsgFromCbor, 4)
			node.monitor.proto.Start()
		}
	}
	// A connection whose peer closes it while every stock protocol is idle
	// reports no error, so the chain-sync instance's end is watched as well.
	go func() {
		select {
		case err, ok := <-errorChan:
			if !ok || err == nil {
				err = errors.New("node connection closed")
			}
			node.die(err)
		case <-node.chainSync.proto.DoneChan():
			node.die(errors.New("node connection closed"))
		case <-node.dead:
		}
	}()
	return node, nil
}
