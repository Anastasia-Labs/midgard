package main

import (
	"bufio"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"io"
	"net"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	ouroboros "github.com/blinklabs-io/gouroboros"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
)

type serviceFrame struct {
	verb    string
	id      string
	payload string
}

// serviceHarness runs one service over pipes and parses every output frame.
type serviceHarness struct {
	t       *testing.T
	service *exactPointService
	input   *io.PipeWriter
	frames  chan serviceFrame
	status  chan int
}

func startService(t *testing.T, configure func(*exactPointService)) *serviceHarness {
	t.Helper()
	inputReader, inputWriter := io.Pipe()
	outputReader, outputWriter := io.Pipe()
	service := newExactPointService(outputWriter)
	if configure != nil {
		configure(service)
	}
	h := &serviceHarness{t: t, service: service, input: inputWriter, frames: make(chan serviceFrame, 64), status: make(chan int, 1)}
	go func() {
		status := service.serve(inputReader)
		_ = outputWriter.Close()
		h.status <- status
	}()
	go func() {
		scanner := bufio.NewScanner(outputReader)
		scanner.Buffer(make([]byte, 0, 1024), 16*1024*1024)
		for scanner.Scan() {
			verb, rest, _ := strings.Cut(scanner.Text(), " ")
			id, payload, _ := strings.Cut(rest, " ")
			h.frames <- serviceFrame{verb: verb, id: id, payload: payload}
		}
		close(h.frames)
	}()
	t.Cleanup(func() { _ = inputWriter.Close() })
	return h
}

func (h *serviceHarness) send(line string) {
	h.t.Helper()
	if _, err := io.WriteString(h.input, line+"\n"); err != nil {
		h.t.Fatal(err)
	}
}

func (h *serviceHarness) next() serviceFrame {
	h.t.Helper()
	select {
	case frame, ok := <-h.frames:
		if !ok {
			h.t.Fatal("service output ended")
		}
		return frame
	case <-time.After(5 * time.Second):
		h.t.Fatal("service frame timeout")
	}
	return serviceFrame{}
}

func (h *serviceHarness) exit() int {
	h.t.Helper()
	select {
	case status := <-h.status:
		return status
	case <-time.After(10 * time.Second):
		h.t.Fatal("service did not exit")
	}
	return -1
}

func startupLine(t *testing.T, config startupConfig) string {
	t.Helper()
	encoded, err := canonicalJSON(config)
	if err != nil {
		t.Fatal(err)
	}
	return string(encoded)
}

func errorCode(t *testing.T, payload string) string {
	t.Helper()
	var event errorEvent
	if err := json.Unmarshal([]byte(payload), &event); err != nil || event.Kind != "error" || event.SchemaVersion != schemaVersion {
		t.Fatalf("not a terminal error line: %q", payload)
	}
	return event.Code
}

func TestExactPointServiceRefusesInvalidAndNonExactStartups(t *testing.T) {
	config, _, _ := exactFixture(t)
	stream := validStartup(t)
	h := startService(t, func(s *exactPointService) {
		s.run = func(startupConfig, []byte, *canonicalWriter, io.Writer, <-chan struct{}) int {
			t.Error("refused startup reached the node")
			return 0
		}
	})
	noncanonical := strings.Replace(startupLine(t, config), `{"authorityNodeId"`, `{ "authorityNodeId"`, 1)
	for index, startup := range []string{"{}", noncanonical, startupLine(t, stream)} {
		id := strconv.Itoa(index + 1)
		h.send("open " + id + " " + startup)
		if frame := h.next(); frame.verb != "out" || frame.id != id || errorCode(t, frame.payload) != "invalid_startup" {
			t.Fatalf("refusal frame: %+v", frame)
		}
		if frame := h.next(); frame != (serviceFrame{verb: "end", id: id, payload: "64"}) {
			t.Fatalf("refusal end: %+v", frame)
		}
	}
	_ = h.input.Close()
	if status := h.exit(); status != 0 {
		t.Fatalf("EOF status %d", status)
	}
}

func TestExactPointServiceRejectsProtocolViolations(t *testing.T) {
	config, _, _ := exactFixture(t)
	valid := startupLine(t, config)
	for _, requests := range [][]string{
		{"open 0 " + valid},
		{"open 01 " + valid},
		{"open 1"},
		{"open 1234567890123456 " + valid},
		{"open 2 " + valid, "open 2 " + valid},
		{"open 2 " + valid, "open 1 " + valid},
		{"close 1"},
		{"open 1 " + valid, "close 2"},
		{"close"},
		{"query 1"},
		{"open 1 " + strings.Repeat("x", maxServiceRequestBytes)},
	} {
		h := startService(t, func(s *exactPointService) {
			s.run = func(_ startupConfig, _ []byte, _ *canonicalWriter, _ io.Writer, stop <-chan struct{}) int {
				<-stop
				return 0
			}
		})
		go func() {
			for _, request := range requests {
				if _, err := io.WriteString(h.input, request+"\n"); err != nil {
					return
				}
			}
		}()
		if status := h.exit(); status != serviceProtocolViolation {
			t.Fatalf("%q: status %d", requests, status)
		}
		for frame := range h.frames {
			if frame.verb == "out" || frame.verb == "err" {
				t.Fatalf("%q: unexpected frame %+v", requests, frame)
			}
		}
	}
}

func TestExactPointServiceFramesConcurrentSessionsAndSealsClosedOutput(t *testing.T) {
	config, _, _ := exactFixture(t)
	valid := startupLine(t, config)
	var lateWrites sync.WaitGroup
	h := startService(t, func(s *exactPointService) {
		s.run = func(config startupConfig, _ []byte, writer *canonicalWriter, diagnostics io.Writer, stop <-chan struct{}) int {
			_, _ = diagnostics.Write([]byte("diagnostic\nfor " + config.Operation.Target.BlockNo + "\n"))
			if err := writer.write(errorEvent{Code: "session_" + config.Operation.Target.BlockNo, Kind: "error", SchemaVersion: schemaVersion}); err != nil {
				t.Error(err)
			}
			<-stop
			// A callback abandoned by the owner's close cannot reach stdout.
			lateWrites.Add(1)
			go func() {
				defer lateWrites.Done()
				time.Sleep(50 * time.Millisecond)
				if err := writer.write(errorEvent{Code: "late", Kind: "error", SchemaVersion: schemaVersion}); err == nil {
					t.Error("late session write admitted")
				}
			}()
			writer.seal()
			return 0
		}
	})
	second := config
	target := *config.Operation.Target
	second.Operation.Target = &target
	h.send("open 7 " + valid)
	h.send("open 9 " + startupLine(t, second))
	seen := map[string][]serviceFrame{}
	for len(seen["7"]) < 2 || len(seen["9"]) < 2 {
		frame := h.next()
		seen[frame.id] = append(seen[frame.id], frame)
	}
	for _, id := range []string{"7", "9"} {
		frames := seen[id]
		if frames[0].verb != "err" || frames[1].verb != "out" {
			t.Fatalf("session %s order: %+v", id, frames)
		}
		decoded, err := base64.StdEncoding.DecodeString(frames[0].payload)
		if err != nil || string(decoded) != "diagnostic\nfor "+config.Operation.Target.BlockNo+"\n" {
			t.Fatalf("session %s stderr: %q %v", id, decoded, err)
		}
		if errorCode(t, frames[1].payload) != "session_"+config.Operation.Target.BlockNo {
			t.Fatalf("session %s stdout: %+v", id, frames[1])
		}
	}
	h.send("close 7")
	if frame := h.next(); frame != (serviceFrame{verb: "end", id: "7", payload: "0"}) {
		t.Fatalf("close end: %+v", frame)
	}
	// Closing an ended session is idempotent; the other session stays live.
	h.send("close 7")
	_ = h.input.Close()
	if frame := h.next(); frame != (serviceFrame{verb: "end", id: "9", payload: "0"}) {
		t.Fatalf("EOF end: %+v", frame)
	}
	if status := h.exit(); status != 0 {
		t.Fatalf("EOF status %d", status)
	}
	lateWrites.Wait()
	for frame := range h.frames {
		t.Fatalf("frame after end: %+v", frame)
	}
}

func TestExactPointServiceBoundsConcurrentSessions(t *testing.T) {
	config, _, _ := exactFixture(t)
	valid := startupLine(t, config)
	h := startService(t, func(s *exactPointService) {
		s.run = func(_ startupConfig, _ []byte, _ *canonicalWriter, _ io.Writer, stop <-chan struct{}) int {
			<-stop
			return 0
		}
	})
	for id := 1; id <= maxServiceSessions+1; id++ {
		h.send("open " + strconv.Itoa(id) + " " + valid)
	}
	limit := strconv.Itoa(maxServiceSessions + 1)
	if frame := h.next(); frame.id != limit || errorCode(t, frame.payload) != "service_session_limit" {
		t.Fatalf("limit frame: %+v", frame)
	}
	if frame := h.next(); frame != (serviceFrame{verb: "end", id: limit, payload: "69"}) {
		t.Fatalf("limit end: %+v", frame)
	}
	_ = h.input.Close()
	ended := 0
	for frame := range h.frames {
		if frame.verb != "end" || frame.payload != "0" {
			t.Fatalf("shutdown frame: %+v", frame)
		}
		ended++
	}
	if ended != maxServiceSessions {
		t.Fatalf("ended %d sessions", ended)
	}
	if status := h.exit(); status != 0 {
		t.Fatalf("EOF status %d", status)
	}
}

// peerObservedConn records the node side observing its client's socket close.
type peerObservedConn struct {
	net.Conn
	closed chan struct{}
	once   sync.Once
}

func (c *peerObservedConn) Read(buffer []byte) (int, error) {
	n, err := c.Conn.Read(buffer)
	if err != nil {
		c.once.Do(func() { close(c.closed) })
	}
	return n, err
}

// One real session: an actual node-to-client chain-sync server on a Unix
// socket answers the exact query; the service frames ready, the target block
// and, on close, the session end while the node connection is released.
func TestExactPointServiceRunsActualNodeSession(t *testing.T) {
	config, raw, block := exactFixture(t)
	socketPath := filepath.Join(t.TempDir(), "node.socket")
	listener, err := net.Listen("unix", socketPath)
	if err != nil {
		t.Fatal(err)
	}
	defer listener.Close()
	config.SocketPath = socketPath
	// The query deadline cannot be what releases the node connection.
	config.Operation.TimeoutMs = 60000
	parent, _ := pointFromStartup(config.Intersection)
	nodeTip := chainsync.Tip{Point: pcommon.NewPoint(block.SlotNumber()+5000, block.Hash().Bytes()), BlockNumber: block.BlockNumber() + 5000}
	peerClosed := make(chan struct{})
	go func() {
		conn, err := listener.Accept()
		if err != nil {
			return
		}
		serverErrors := make(chan error, 8)
		observed := &peerObservedConn{Conn: conn, closed: peerClosed}
		server, err := ouroboros.New(ouroboros.WithConnection(observed), ouroboros.WithServer(true), ouroboros.WithNodeToNode(false), ouroboros.WithNetworkMagic(1), ouroboros.WithErrorChan(serverErrors), ouroboros.WithChainSyncConfig(chainsync.Config{
			FindIntersectFunc: func(_ chainsync.CallbackContext, points []pcommon.Point) (pcommon.Point, chainsync.Tip, error) {
				if len(points) == 0 {
					return pcommon.Point{}, nodeTip, chainsync.ErrIntersectNotFound
				}
				return parent, nodeTip, nil
			},
			RequestNextFunc: func(ctx chainsync.CallbackContext) error {
				return ctx.Server.RollForward(7, raw, nodeTip)
			},
		}))
		if err != nil {
			return
		}
		defer server.Close()
		<-peerClosed
	}()
	h := startService(t, nil)
	line := startupLine(t, config)
	h.send("open 1 " + line)
	var ready readyEvent
	frame := h.next()
	if frame.verb != "out" || frame.id != "1" || json.Unmarshal([]byte(frame.payload), &ready) != nil || ready.Kind != "ready" {
		t.Fatalf("ready frame: %+v", frame)
	}
	if ready.Operation.Target.BlockHash != block.Hash().String() || ready.CurrentTip.BlockNo != strconv.FormatUint(block.BlockNumber()+5000, 10) {
		t.Fatalf("ready identity: %+v", ready)
	}
	var forward rollForwardEvent
	frame = h.next()
	if frame.verb != "out" || json.Unmarshal([]byte(frame.payload), &forward) != nil || forward.Kind != "roll_forward" || forward.BlockHash != block.Hash().String() || forward.RawBlockCBOR != hex.EncodeToString(raw) {
		t.Fatalf("target frame: %+v", frame.verb)
	}
	h.send("close 1")
	for {
		frame = h.next()
		if frame.verb == "end" {
			break
		}
		if frame.verb != "err" {
			t.Fatalf("frame after close: %+v", frame)
		}
	}
	if frame != (serviceFrame{verb: "end", id: "1", payload: "0"}) {
		t.Fatalf("session end: %+v", frame)
	}
	select {
	case <-peerClosed:
	case <-time.After(5 * time.Second):
		t.Fatal("closed session kept its node connection")
	}
	_ = h.input.Close()
	if status := h.exit(); status != 0 {
		t.Fatalf("EOF status %d", status)
	}
}
