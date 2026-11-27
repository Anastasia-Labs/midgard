package main

import (
	"bytes"
	"encoding/binary"
	"errors"
	"testing"
)

func frameLengths(header, payload uint32) []byte {
	var lengths [frameLengthBytes]byte
	binary.BigEndian.PutUint32(lengths[0:4], header)
	binary.BigEndian.PutUint32(lengths[4:8], payload)
	return lengths[:]
}

// A frame at either byte bound is admitted, one byte more is refused, on both
// the reading and the writing side.
func TestFrameBoundsAdmitTheMaximumAndRefuseOneMore(t *testing.T) {
	payload := bytes.Repeat([]byte{0xa5}, maxPayloadBytes)
	payload[0], payload[len(payload)-1] = 1, 2
	var out bytes.Buffer
	if err := newFrameWriter(&out).write(map[string]any{"type": "cs_roll_forward"}, payload); err != nil {
		t.Fatalf("a maximal payload was refused: %v", err)
	}
	got, err := readFrame(&out)
	if err != nil {
		t.Fatalf("a maximal payload was not read back: %v", err)
	}
	if !bytes.Equal(got.payload, payload) {
		t.Fatal("the maximal payload did not round-trip")
	}
	if err := newFrameWriter(&out).write(map[string]any{"type": "cs_roll_forward"}, make([]byte, maxPayloadBytes+1)); !errors.Is(err, errFrameBounds) {
		t.Fatalf("the writer admitted a payload over the bound: %v", err)
	}
	if _, err := readFrame(bytes.NewReader(frameLengths(1, maxPayloadBytes+1))); !errors.Is(err, errFrameBounds) {
		t.Fatalf("the reader admitted a payload over the bound: %v", err)
	}

	// A byte string of 256..65535 bytes has a fixed 3-byte length prefix,
	// so the header's overhead is measured once and the string sized to it.
	probe, err := headerEncMode.Marshal(map[string]any{"type": "x", "p": make([]byte, 256)})
	if err != nil {
		t.Fatal(err)
	}
	header := map[string]any{"type": "x", "p": make([]byte, maxHeaderBytes-(len(probe)-256))}
	encoded, err := headerEncMode.Marshal(header)
	if err != nil || len(encoded) != maxHeaderBytes {
		t.Fatalf("header fixture is %d bytes, not the bound: %v", len(encoded), err)
	}
	out.Reset()
	if err := newFrameWriter(&out).write(header, nil); err != nil {
		t.Fatalf("a maximal header was refused: %v", err)
	}
	got, err = readFrame(&out)
	if err != nil || !bytes.Equal(got.header, encoded) {
		t.Fatalf("a maximal header was not read back: %v", err)
	}
	header["p"] = make([]byte, len(header["p"].([]byte))+1)
	if err := newFrameWriter(&out).write(header, nil); !errors.Is(err, errFrameBounds) {
		t.Fatalf("the writer admitted a header over the bound: %v", err)
	}
	over := append(frameLengths(maxHeaderBytes+1, 0), make([]byte, maxHeaderBytes+1)...)
	if _, err := readFrame(bytes.NewReader(over)); !errors.Is(err, errFrameBounds) {
		t.Fatalf("the reader admitted a header over the bound: %v", err)
	}
}
