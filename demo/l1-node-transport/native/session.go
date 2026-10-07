package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"math"
	"sync"
	"time"

	"github.com/blinklabs-io/gouroboros/ledger"
)

// Exit statuses. A fatal frame precedes every non-zero status except a
// failure to write to the client.
const (
	exitOrderly       = 0
	exitFatal         = 1
	exitClientMisuse  = 64
	exitNodeAvailable = 69

	requestQueueDepth = 256
	monitorBound      = 30 * time.Second
)

type session struct {
	ctx          context.Context
	cancel       context.CancelFunc
	socketPath   string
	networkMagic uint32
	diagnostics  io.Writer
	out          *frameWriter
	primary      *nodeConn
	pool         *csPool

	mu         sync.Mutex
	streams    map[uint64]*csStream
	lastStream uint64

	lsq     *lsqClient
	submit  *submitClient
	monitor *monitorClient
	queues  [3]chan func()
	endOnce sync.Once
	done    chan struct{}
	status  int
}

const (
	queueLSQ = iota
	queueSubmit
	queueMonitor
)

// finish ends the session with status; the first call wins.
func (s *session) finish(status int) {
	s.end(status, nil)
}

// end ends the session once; the first ending wins, and later faults (the
// node connection closing because the session ended, say) are silent.
func (s *session) end(status int, terminal func()) {
	s.endOnce.Do(func() {
		if terminal != nil {
			terminal()
		}
		s.out.seal()
		s.status = status
		close(s.done)
	})
}

// fatal writes the terminal fatal frame and ends the session.
func (s *session) fatal(code string, cause error) {
	message := "fatal"
	if cause != nil {
		message = cause.Error()
	}
	status := exitFatal
	if code == "client_protocol_violation" || code == "version_unsupported" || code == "malformed_frame" {
		status = exitClientMisuse
	}
	s.end(status, func() {
		_, _ = fmt.Fprintf(s.diagnostics, "l1-node-transport fatal %s: %s\n", code, message)
		_ = s.out.write(fatalHeader{Type: "fatal", Code: code, Message: message}, nil)
	})
}

func (s *session) answerError(id uint64, refusal *requestError) {
	_ = s.out.write(errorHeader{Type: "error", ID: id, Code: refusal.code, Message: refusal.message}, nil)
}

func (s *session) dropStream(id uint64) {
	s.mu.Lock()
	delete(s.streams, id)
	s.mu.Unlock()
}

func (s *session) stream(id uint64) *csStream {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.streams[id]
}

// nodeFault ends the session for a node-side failure of a request.
func (s *session) nodeFault(id uint64, err error) {
	var refusal *requestError
	if errors.As(err, &refusal) {
		s.answerError(id, refusal)
		return
	}
	s.fatal("node_connection_lost", err)
}

func (s *session) enqueue(queue int, id uint64, work func()) {
	select {
	case s.queues[queue] <- work:
	default:
		s.answerError(id, refuse("busy", "too many outstanding requests"))
	}
}

func (s *session) worker(queue chan func()) {
	for {
		select {
		case <-s.done:
			return
		case work := <-queue:
			work()
		}
	}
}

func noPayload(f frame) error {
	if len(f.payload) != 0 {
		return errors.New("this frame type carries no payload")
	}
	return nil
}

// dispatch handles one client frame without blocking on the node.
func (s *session) dispatch(f frame) error {
	kind, err := headerType(f.header)
	if err != nil {
		return err
	}
	if kind != "submit" {
		if err := noPayload(f); err != nil {
			return err
		}
	}
	switch kind {
	case "cs_open":
		var h csOpenHeader
		if err := decodeHeader(f.header, &h); err != nil {
			return err
		}
		return s.openStream(h)
	case "cs_window":
		var h csWindowHeader
		if err := decodeHeader(f.header, &h); err != nil {
			return err
		}
		if st := s.stream(h.Stream); st != nil {
			if refusal := st.setWindow(h.Window); refusal != nil {
				return refusal
			}
		} else if h.Window == 0 || h.Window > maxWindow {
			return refuse("invalid_window", "window must be within 1..%d", maxWindow)
		}
		return nil
	case "cs_ack":
		var h csAckHeader
		if err := decodeHeader(f.header, &h); err != nil {
			return err
		}
		// An ack may race a stream's failure or close; it is then moot.
		if st := s.stream(h.Stream); st != nil {
			if refusal := st.ack(h.Seq); refusal != nil {
				return refusal
			}
		}
		return nil
	case "cs_close":
		var h csCloseHeader
		if err := decodeHeader(f.header, &h); err != nil {
			return err
		}
		if st := s.stream(h.Stream); st != nil {
			st.requestClose(h.ID)
		} else {
			_ = s.out.write(okHeader{Type: "ok", ID: h.ID}, nil)
		}
		return nil
	case "lsq_acquire":
		var h lsqAcquireHeader
		if err := decodeHeader(f.header, &h); err != nil {
			return err
		}
		s.enqueue(queueLSQ, h.ID, func() {
			if err := s.lsq.acquire(h.Point); err != nil {
				s.nodeFault(h.ID, err)
				return
			}
			_ = s.out.write(okHeader{Type: "ok", ID: h.ID}, nil)
		})
		return nil
	case "lsq_release":
		var h lsqReleaseHeader
		if err := decodeHeader(f.header, &h); err != nil {
			return err
		}
		s.enqueue(queueLSQ, h.ID, func() {
			if err := s.lsq.release(); err != nil {
				s.nodeFault(h.ID, err)
				return
			}
			_ = s.out.write(okHeader{Type: "ok", ID: h.ID}, nil)
		})
		return nil
	case "lsq_query":
		var h lsqQueryHeader
		if err := decodeHeader(f.header, &h); err != nil {
			return err
		}
		s.enqueue(queueLSQ, h.ID, func() {
			result, err := s.lsq.run(h)
			if err != nil {
				s.nodeFault(h.ID, err)
				return
			}
			if err := s.out.write(idOnlyHeader{Type: "lsq_result", ID: h.ID}, result); errors.Is(err, errFrameBounds) {
				s.answerError(h.ID, refuse("result_too_large", "the ledger answer exceeds the frame bound"))
			}
		})
		return nil
	case "submit":
		var h submitHeader
		if err := decodeHeader(f.header, &h); err != nil {
			return err
		}
		tx := f.payload
		s.enqueue(queueSubmit, h.ID, func() { s.runSubmit(h, tx) })
		return nil
	case "monitor_has_tx":
		var h monitorHasTxHeader
		if err := decodeHeader(f.header, &h); err != nil {
			return err
		}
		s.enqueue(queueMonitor, h.ID, func() { s.runHasTx(h) })
		return nil
	case "monitor_sizes":
		var h monitorSizesHeader
		if err := decodeHeader(f.header, &h); err != nil {
			return err
		}
		s.enqueue(queueMonitor, h.ID, func() { s.runSizes(h) })
		return nil
	case "hello":
		return errors.New("hello was already answered")
	default:
		return fmt.Errorf("unknown frame type %q", kind)
	}
}

func (s *session) openStream(h csOpenHeader) error {
	if h.StartSeq == nil {
		return errors.New("cs_open needs startSeq")
	}
	if h.Window == 0 || h.Window > maxWindow {
		return refuse("invalid_window", "window must be within 1..%d", maxWindow)
	}
	if *h.StartSeq > math.MaxUint64-1<<32 {
		return errors.New("startSeq leaves no sequence space")
	}
	s.mu.Lock()
	if h.Stream <= s.lastStream {
		s.mu.Unlock()
		return fmt.Errorf("stream id %d is not above the last opened stream %d", h.Stream, s.lastStream)
	}
	s.lastStream = h.Stream
	s.mu.Unlock()
	if len(h.Points) == 0 || len(h.Points) > maxIntersectPoints {
		s.answerError(h.ID, refuse("invalid_points", "cs_open needs 1..%d points", maxIntersectPoints))
		return nil
	}
	acked := *h.StartSeq
	if h.AckedSeq != nil {
		if *h.AckedSeq > *h.StartSeq {
			return errors.New("ackedSeq is above startSeq")
		}
		acked = *h.AckedSeq
	}
	st := &csStream{
		id: h.Stream, session: s, wake: make(chan struct{}, 1),
		window: h.Window, acked: acked, lastSeq: *h.StartSeq,
	}
	s.mu.Lock()
	s.streams[h.Stream] = st
	s.mu.Unlock()
	go st.run(h)
	return nil
}

func (s *session) runSubmit(h submitHeader, tx []byte) {
	if len(tx) == 0 {
		s.answerError(h.ID, refuse("invalid_request", "submit needs the transaction as payload"))
		return
	}
	var era uint16
	if h.Era != nil {
		if *h.Era > math.MaxUint16 {
			s.answerError(h.ID, refuse("invalid_request", "era is out of range"))
			return
		}
		era = uint16(*h.Era)
	} else {
		txType, err := ledger.DetermineTransactionType(tx)
		if err != nil {
			s.answerError(h.ID, refuse("tx_undecodable", "transaction era cannot be determined: %v", err))
			return
		}
		era = uint16(txType)
	}
	accepted, reason, err := s.submit.submit(era, tx)
	if err != nil {
		s.nodeFault(h.ID, err)
		return
	}
	if accepted {
		_ = s.out.write(idOnlyHeader{Type: "submit_accepted", ID: h.ID}, nil)
		return
	}
	_ = s.out.write(idOnlyHeader{Type: "submit_rejected", ID: h.ID}, reason)
}

// monitorCall runs one LocalTxMonitor exchange on a fresh mempool snapshot
// (HasTx and GetSizes acquire one) and releases the snapshot afterwards, so
// each answer reflects the mempool at the time of the request.
func (s *session) monitorCall(id uint64, call func() error) {
	if s.monitor == nil {
		s.answerError(id, refuse("monitor_unavailable", "the node did not negotiate LocalTxMonitor"))
		return
	}
	result := make(chan error, 1)
	go func() {
		result <- call()
	}()
	select {
	case err := <-result:
		if err != nil {
			s.fatal("node_connection_lost", err)
		}
	case <-s.primary.dead:
		s.fatal("node_connection_lost", s.primary.deathCause())
	case <-time.After(monitorBound):
		s.fatal("node_unresponsive", errors.New("LocalTxMonitor did not answer within its bound"))
	}
}

func (s *session) runHasTx(h monitorHasTxHeader) {
	if len(h.TxID) != 32 {
		s.answerError(h.ID, refuse("invalid_request", "txId is not 32 bytes"))
		return
	}
	var has bool
	s.monitorCall(h.ID, func() error {
		var err error
		has, err = s.monitor.hasTx(h.TxID)
		return err
	})
	select {
	case <-s.done:
	default:
		_ = s.out.write(monitorHasTxResultHeader{Type: "monitor_has_tx_result", ID: h.ID, Has: has}, nil)
	}
}

func (s *session) runSizes(h monitorSizesHeader) {
	var capacity, size, count uint32
	s.monitorCall(h.ID, func() error {
		var err error
		capacity, size, count, err = s.monitor.sizes()
		return err
	})
	select {
	case <-s.done:
	default:
		_ = s.out.write(monitorSizesResultHeader{
			Type: "monitor_sizes_result", ID: h.ID,
			Capacity: uint64(capacity), Size: uint64(size), TxCount: uint64(count),
		}, nil)
	}
}

// runSession serves one client over in/out until the client closes its
// input, a signal arrives (stop), or a fatal fault. It returns the exit
// status.
func runSession(in io.Reader, out io.Writer, diagnostics io.Writer, stop <-chan struct{}) int {
	writer := newFrameWriter(out)
	first, err := readFrame(in)
	if err != nil {
		if errors.Is(err, io.EOF) {
			return exitOrderly
		}
		_ = writer.write(fatalHeader{Type: "fatal", Code: "malformed_frame", Message: err.Error()}, nil)
		return exitClientMisuse
	}
	var hello helloHeader
	kind, err := headerType(first.header)
	if err == nil && kind != "hello" {
		err = fmt.Errorf("first frame is %q, not hello", kind)
	}
	if err == nil {
		err = decodeHeader(first.header, &hello)
	}
	if err == nil {
		err = noPayload(first)
	}
	if err == nil && (hello.Version == nil || hello.NetworkMagic == nil || hello.SocketPath == "") {
		err = errors.New("hello needs version, socketPath and networkMagic")
	}
	if err != nil {
		_ = writer.write(fatalHeader{Type: "fatal", Code: "malformed_frame", Message: err.Error()}, nil)
		return exitClientMisuse
	}
	if *hello.Version != protocolVersion {
		_ = writer.write(fatalHeader{
			Type: "fatal", Code: "version_unsupported",
			Message: fmt.Sprintf("frame protocol version %d is not supported; this sidecar speaks %d", *hello.Version, protocolVersion),
		}, nil)
		return exitClientMisuse
	}
	if *hello.NetworkMagic == 0 || *hello.NetworkMagic > math.MaxUint32 {
		_ = writer.write(fatalHeader{Type: "fatal", Code: "malformed_frame", Message: "networkMagic is out of range"}, nil)
		return exitClientMisuse
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	go func() {
		select {
		case <-stop:
			cancel()
		case <-ctx.Done():
		}
	}()
	primary, err := dialNode(ctx, hello.SocketPath, uint32(*hello.NetworkMagic), true, diagnostics)
	if err != nil {
		code := "node_unreachable"
		var dialFailure *dialError
		if errors.As(err, &dialFailure) {
			code = dialFailure.code
		}
		_, _ = fmt.Fprintf(diagnostics, "l1-node-transport %s: %v\n", code, err)
		_ = writer.write(fatalHeader{Type: "fatal", Code: code, Message: err.Error()}, nil)
		select {
		case <-stop:
			return exitOrderly
		default:
		}
		return exitNodeAvailable
	}
	s := &session{
		ctx: ctx, cancel: cancel, socketPath: hello.SocketPath, networkMagic: uint32(*hello.NetworkMagic),
		diagnostics: diagnostics, out: writer, primary: primary,
		streams: map[uint64]*csStream{}, done: make(chan struct{}),
	}
	s.pool = &csPool{session: s, primaryFree: true}
	s.lsq = &lsqClient{raw: primary.lsq, node: primary}
	s.submit = &submitClient{raw: primary.submit, node: primary}
	if primary.monitor != nil {
		s.monitor = &monitorClient{raw: primary.monitor, node: primary}
	}
	for i := range s.queues {
		s.queues[i] = make(chan func(), requestQueueDepth)
		go s.worker(s.queues[i])
	}
	version, _ := primary.conn.ProtocolVersion()
	if err := writer.write(helloOkHeader{Type: "hello_ok", Version: protocolVersion, NodeToClientVersion: uint64(version)}, nil); err != nil {
		primary.close()
		return exitFatal
	}
	go func() {
		select {
		case <-primary.dead:
			s.fatal("node_connection_lost", primary.deathCause())
		case <-s.done:
		}
	}()
	go func() {
		for {
			f, err := readFrame(in)
			if err != nil {
				if errors.Is(err, io.EOF) {
					s.finish(exitOrderly)
				} else {
					s.fatal("malformed_frame", err)
				}
				return
			}
			if err := s.dispatch(f); err != nil {
				var refusal *requestError
				if errors.As(err, &refusal) {
					s.fatal("client_protocol_violation", refusal)
				} else {
					s.fatal("client_protocol_violation", err)
				}
				return
			}
			select {
			case <-s.done:
				return
			default:
			}
		}
	}()
	select {
	case <-s.done:
	case <-stop:
		s.finish(exitOrderly)
	}
	cancel()
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		s.pool.closeAll()
	}()
	primary.close()
	wg.Wait()
	return s.status
}
