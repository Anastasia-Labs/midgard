package main

import (
	"bufio"
	"bytes"
	"encoding/base64"
	"encoding/json"
	"errors"
	"io"
	"strconv"
	"sync"
	"time"
)

// The exact-point service multiplexes independent exact-point sessions over
// one helper process. Each session keeps its own node connection, absolute
// deadline and ingress bound, and frames exactly the stdout lines, stderr
// bytes and exit status one per-query helper process produced.
//
// Owner to helper (stdin):  "open <id> <startup line>\n", "close <id>\n"
// Helper to owner (stdout): "out <id> <stdout line>\n",
//
//	"err <id> <base64 stderr bytes>\n", "end <id> <exit status>\n"
//
// Ids are canonical naturals the owner allocates in strictly increasing
// order. A malformed request ends the service; stdin EOF closes every session
// and exits, so the helper cannot outlive its owner.
const (
	exactPointServiceFlag     = "--exact-point-service"
	maxServiceSessions        = 256
	maxServiceRequestBytes    = maxStartupBytes + 64
	maxServiceStderrChunk     = 4096
	maxServiceSessionIDDigits = 15
	serviceProtocolViolation  = 65
	serviceOutputFailed       = 74
	serviceShutdownBound      = 5 * time.Second
)

type serviceOutput struct {
	mutex  sync.Mutex
	writer io.Writer
	failed bool
}

type sessionFrames struct {
	output *serviceOutput
	id     []byte
	ended  bool // guarded by output.mutex
}

func (o *serviceOutput) frameLocked(parts ...[]byte) error {
	if o.failed {
		return errors.New("native exact-point service output failed")
	}
	frame := bytes.Join(parts, nil)
	if _, err := o.writer.Write(frame); err != nil {
		o.failed = true
		return err
	}
	return nil
}

// Write receives exactly one newline-terminated canonical JSON line per call
// from json.Encoder.Encode.
func (f *sessionFrames) Write(line []byte) (int, error) {
	if len(line) == 0 || line[len(line)-1] != '\n' || bytes.IndexByte(line, '\n') != len(line)-1 {
		return 0, errors.New("native exact-point session line is not framed")
	}
	f.output.mutex.Lock()
	defer f.output.mutex.Unlock()
	if f.ended {
		return 0, errWriterSealed
	}
	if err := f.output.frameLocked([]byte("out "), f.id, []byte(" "), line); err != nil {
		return 0, err
	}
	return len(line), nil
}

type sessionDiagnostics struct{ frames *sessionFrames }

func (d sessionDiagnostics) Write(chunk []byte) (int, error) {
	f := d.frames
	f.output.mutex.Lock()
	defer f.output.mutex.Unlock()
	if f.ended {
		return len(chunk), nil
	}
	for offset := 0; offset < len(chunk); offset += maxServiceStderrChunk {
		end := min(offset+maxServiceStderrChunk, len(chunk))
		encoded := base64.StdEncoding.EncodeToString(chunk[offset:end])
		if err := f.output.frameLocked([]byte("err "), f.id, []byte(" "), []byte(encoded), []byte("\n")); err != nil {
			return offset, err
		}
	}
	return len(chunk), nil
}

func (f *sessionFrames) end(status int) {
	f.output.mutex.Lock()
	defer f.output.mutex.Unlock()
	if f.ended {
		return
	}
	f.ended = true
	_ = f.output.frameLocked([]byte("end "), f.id, []byte(" "), []byte(strconv.Itoa(status)), []byte("\n"))
}

type serviceSession struct {
	stop     chan struct{}
	stopOnce sync.Once
}

func (s *serviceSession) close() { s.stopOnce.Do(func() { close(s.stop) }) }

type exactPointService struct {
	output   *serviceOutput
	mutex    sync.Mutex
	sessions map[uint64]*serviceSession
	lastID   uint64
	running  sync.WaitGroup
	// run is replaceable in tests; production sessions run runChainSync.
	run func(config startupConfig, canonical []byte, writer *canonicalWriter, diagnostics io.Writer, stop <-chan struct{}, onReady func()) int
}

func newExactPointService(writer io.Writer) *exactPointService {
	return &exactPointService{
		output:   &serviceOutput{writer: writer},
		sessions: map[uint64]*serviceSession{},
		run:      runChainSync,
	}
}

func parseServiceID(value []byte) (uint64, bool) {
	if len(value) == 0 || len(value) > maxServiceSessionIDDigits || !canonicalNatural(string(value)) {
		return 0, false
	}
	id, err := strconv.ParseUint(string(value), 10, 64)
	return id, err == nil && id > 0
}

// shutdown stops every session and waits, within a bound, for each to frame
// its end. The service exits after it returns either way.
func (s *exactPointService) shutdown() {
	s.mutex.Lock()
	for _, session := range s.sessions {
		session.close()
	}
	s.mutex.Unlock()
	drained := make(chan struct{})
	go func() {
		s.running.Wait()
		close(drained)
	}()
	select {
	case <-drained:
	case <-time.After(serviceShutdownBound):
	}
}

func (s *exactPointService) open(id uint64, startup []byte) {
	frames := &sessionFrames{output: s.output, id: []byte(strconv.FormatUint(id, 10))}
	writer := &canonicalWriter{encoder: json.NewEncoder(frames)}
	config, canonical, err := parseStartupLine(startup)
	if err != nil || config.Operation.Kind != "exact_point" {
		_ = writer.write(errorEvent{Code: "invalid_startup", Kind: "error", SchemaVersion: schemaVersion})
		frames.end(64)
		return
	}
	s.mutex.Lock()
	if len(s.sessions) >= maxServiceSessions {
		s.mutex.Unlock()
		_ = writer.write(errorEvent{Code: "service_session_limit", Kind: "error", SchemaVersion: schemaVersion})
		frames.end(69)
		return
	}
	session := &serviceSession{stop: make(chan struct{})}
	s.sessions[id] = session
	s.running.Add(1)
	s.mutex.Unlock()
	go func() {
		defer s.running.Done()
		status := s.run(config, canonical, writer, sessionDiagnostics{frames: frames}, session.stop, nil)
		s.mutex.Lock()
		delete(s.sessions, id)
		s.mutex.Unlock()
		frames.end(status)
	}()
}

// serve reads owner requests until EOF and returns the service exit status.
func (s *exactPointService) serve(input io.Reader) int {
	reader := bufio.NewReaderSize(input, maxServiceRequestBytes+1)
	for {
		line, err := reader.ReadSlice('\n')
		if err == io.EOF && len(line) == 0 {
			s.shutdown()
			return 0
		}
		if err != nil {
			s.shutdown()
			return serviceProtocolViolation
		}
		verb, rest, _ := bytes.Cut(line[:len(line)-1], []byte(" "))
		switch string(verb) {
		case "open":
			rawID, startup, found := bytes.Cut(rest, []byte(" "))
			id, ok := parseServiceID(rawID)
			if !found || !ok || id <= s.lastID {
				s.shutdown()
				return serviceProtocolViolation
			}
			s.lastID = id
			s.open(id, startup)
		case "close":
			id, ok := parseServiceID(rest)
			if !ok || id > s.lastID {
				s.shutdown()
				return serviceProtocolViolation
			}
			s.mutex.Lock()
			session := s.sessions[id]
			s.mutex.Unlock()
			// An already-ended session has framed its end; closing it is a no-op.
			if session != nil {
				session.close()
			}
		default:
			s.shutdown()
			return serviceProtocolViolation
		}
		s.output.mutex.Lock()
		failed := s.output.failed
		s.output.mutex.Unlock()
		if failed {
			s.shutdown()
			return serviceOutputFailed
		}
	}
}
