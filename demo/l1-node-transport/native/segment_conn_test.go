package main

import (
	"bytes"
	"encoding/binary"
	"errors"
	"github.com/blinklabs-io/gouroboros/muxer"
	"net"
	"os"
	"strconv"
	"testing"
	"testing/synctest"
	"time"
)

func nativeMuxerFixture(t *testing.T, transport net.Conn) (*muxer.Muxer, chan *muxer.Segment) {
	t.Helper()
	m := muxer.New(transport)
	_, received, _ := m.RegisterProtocol(5, muxer.ProtocolRoleInitiator)
	m.Start()
	t.Cleanup(m.Stop)
	return m, received
}

func writeNativeSegment(t *testing.T, peer net.Conn, payload []byte) {
	t.Helper()
	segment := muxer.NewSegment(5, payload, true)
	var encoded bytes.Buffer
	if err := binary.Write(&encoded, binary.BigEndian, segment.SegmentHeader); err != nil {
		t.Fatal(err)
	}
	encoded.Write(payload)
	if _, err := peer.Write(encoded.Bytes()); err != nil {
		t.Fatal(err)
	}
}

func assertNativeMuxerHealthy(t *testing.T, m *muxer.Muxer) {
	t.Helper()
	select {
	case err := <-m.ErrorChan():
		t.Fatalf("muxer stopped: %v", err)
	default:
	}
}

func TestPinnedMuxerIdleDeadlineReproduction(t *testing.T) {
	synctest.Test(t, func(t *testing.T) {
		left, right := net.Pipe()
		defer right.Close()
		m, _ := nativeMuxerFixture(t, left)
		time.Sleep(121 * time.Second)
		if err := <-m.ErrorChan(); !errors.Is(err, os.ErrDeadlineExceeded) {
			t.Fatalf("expected original idle timeout, got %v", err)
		}
	})
}

func TestStreamMuxerWaitsBetweenSegmentsAndPreservesBackpressure(t *testing.T) {
	synctest.Test(t, func(t *testing.T) {
		left, right := net.Pipe()
		defer right.Close()
		m, received := nativeMuxerFixture(t, &streamSegmentConn{Conn: left})
		for i := range 3 {
			time.Sleep(130 * time.Second)
			assertNativeMuxerHealthy(t, m)
			writeNativeSegment(t, right, []byte{byte(i)})
			if got := <-received; !bytes.Equal(got.Payload, []byte{byte(i)}) {
				t.Fatalf("segment after idle: %v", got)
			}
		}
		// Stop consuming until the muxer's bounded delivery channel fills. The
		// last segment has been read but cannot be delivered to its owner.
		for i := range cap(received) + 1 {
			writeNativeSegment(t, right, []byte{byte(i)})
		}
		time.Sleep(130 * time.Second)
		assertNativeMuxerHealthy(t, m)
		for i := range cap(received) + 1 {
			if got := <-received; !bytes.Equal(got.Payload, []byte{byte(i)}) {
				t.Fatalf("backpressure reordered segment %d: %v", i, got)
			}
		}
		time.Sleep(130 * time.Second)
		assertNativeMuxerHealthy(t, m)
		writeNativeSegment(t, right, []byte("resumed"))
		if got := <-received; string(got.Payload) != "resumed" {
			t.Fatalf("stream did not resume: %v", got)
		}
	})
}

func TestStreamMuxerStillBoundsPartialSegments(t *testing.T) {
	for _, partialPayload := range []bool{false, true} {
		t.Run(strconv.FormatBool(partialPayload), func(t *testing.T) {
			synctest.Test(t, func(t *testing.T) {
				left, right := net.Pipe()
				defer right.Close()
				m, _ := nativeMuxerFixture(t, &streamSegmentConn{Conn: left})
				time.Sleep(130 * time.Second)
				var prefix bytes.Buffer
				if partialPayload {
					segment := muxer.NewSegment(5, []byte("abc"), true)
					if err := binary.Write(&prefix, binary.BigEndian, segment.SegmentHeader); err != nil {
						t.Fatal(err)
					}
				}
				prefix.WriteByte(0)
				if _, err := right.Write(prefix.Bytes()); err != nil {
					t.Fatal(err)
				}
				time.Sleep(121 * time.Second)
				if err := <-m.ErrorChan(); !errors.Is(err, os.ErrDeadlineExceeded) {
					t.Fatalf("incomplete segment did not time out: %v", err)
				}
			})
		})
	}
}

func TestStreamMuxerIdleCloseStillTerminates(t *testing.T) {
	for _, ownerClose := range []bool{false, true} {
		t.Run(strconv.FormatBool(ownerClose), func(t *testing.T) {
			synctest.Test(t, func(t *testing.T) {
				left, right := net.Pipe()
				defer right.Close()
				m, _ := nativeMuxerFixture(t, &streamSegmentConn{Conn: left})
				time.Sleep(130 * time.Second)
				if ownerClose {
					m.Stop()
				} else {
					_ = right.Close()
				}
				// Draining until closure also proves all muxer workers terminated.
				for range m.ErrorChan() {
				}
			})
		})
	}
}
