package main

import (
	"bufio"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"sync"

	"github.com/fxamacker/cbor/v2"
)

// Frame layout (see README.md, "Frame protocol"):
//
//	u32 big-endian header length | u32 big-endian payload length |
//	header (one CBOR map with text keys) | payload (raw bytes)
//
// The header is small control data. The payload carries raw node bytes
// (blocks, transactions, ledger answers) and is never re-encoded.
const (
	protocolVersion  = 1
	maxHeaderBytes   = 64 * 1024
	maxPayloadBytes  = 64 * 1024 * 1024
	frameLengthBytes = 8
)

var (
	headerDecMode cbor.DecMode
	headerEncMode cbor.EncMode
)

func init() {
	var err error
	headerDecMode, err = cbor.DecOptions{
		DupMapKey:         cbor.DupMapKeyEnforcedAPF,
		IndefLength:       cbor.IndefLengthForbidden,
		TagsMd:            cbor.TagsForbidden,
		ExtraReturnErrors: cbor.ExtraDecErrorUnknownField,
		MaxNestedLevels:   16,
		MaxArrayElements:  4096,
		MaxMapPairs:       64,
	}.DecMode()
	if err != nil {
		panic(err)
	}
	headerEncMode, err = cbor.CoreDetEncOptions().EncMode()
	if err != nil {
		panic(err)
	}
}

type frame struct {
	header  []byte
	payload []byte
}

var errFrameBounds = errors.New("frame exceeds its byte bound")

// readFrame reads one whole frame. io.EOF is returned only at a frame
// boundary; a frame truncated mid-way is io.ErrUnexpectedEOF.
func readFrame(r io.Reader) (frame, error) {
	var lengths [frameLengthBytes]byte
	if _, err := io.ReadFull(r, lengths[:]); err != nil {
		return frame{}, err
	}
	headerLen := binary.BigEndian.Uint32(lengths[0:4])
	payloadLen := binary.BigEndian.Uint32(lengths[4:8])
	if headerLen == 0 || headerLen > maxHeaderBytes || payloadLen > maxPayloadBytes {
		return frame{}, errFrameBounds
	}
	buffer := make([]byte, int(headerLen)+int(payloadLen))
	if _, err := io.ReadFull(r, buffer); err != nil {
		if errors.Is(err, io.EOF) {
			err = io.ErrUnexpectedEOF
		}
		return frame{}, err
	}
	return frame{header: buffer[:headerLen], payload: buffer[headerLen:]}, nil
}

// frameWriter serialises whole frames from concurrent producers.
type frameWriter struct {
	mu     sync.Mutex
	out    *bufio.Writer
	sealed bool
}

func newFrameWriter(w io.Writer) *frameWriter {
	return &frameWriter{out: bufio.NewWriterSize(w, 256*1024)}
}

var errWriterSealed = errors.New("frame writer is sealed")

func (w *frameWriter) write(header any, payload []byte) error {
	encoded, err := headerEncMode.Marshal(header)
	if err != nil {
		return fmt.Errorf("encode frame header: %w", err)
	}
	if len(encoded) > maxHeaderBytes || len(payload) > maxPayloadBytes {
		return errFrameBounds
	}
	var lengths [frameLengthBytes]byte
	binary.BigEndian.PutUint32(lengths[0:4], uint32(len(encoded)))
	binary.BigEndian.PutUint32(lengths[4:8], uint32(len(payload)))
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.sealed {
		return errWriterSealed
	}
	if _, err := w.out.Write(lengths[:]); err != nil {
		return err
	}
	if _, err := w.out.Write(encoded); err != nil {
		return err
	}
	if _, err := w.out.Write(payload); err != nil {
		return err
	}
	return w.out.Flush()
}

// seal refuses every later frame, so nothing follows a terminal frame.
func (w *frameWriter) seal() {
	w.mu.Lock()
	w.sealed = true
	w.mu.Unlock()
}

// headerType reads only the "type" key of a header, leaving the strict,
// type-specific decode to the handler.
func headerType(raw []byte) (string, error) {
	var fields map[string]cbor.RawMessage
	if err := headerDecMode.Unmarshal(raw, &fields); err != nil {
		return "", fmt.Errorf("frame header is not a CBOR map: %w", err)
	}
	rawType, ok := fields["type"]
	if !ok {
		return "", errors.New("frame header has no type")
	}
	var kind string
	if err := headerDecMode.Unmarshal(rawType, &kind); err != nil || kind == "" {
		return "", errors.New("frame header type is not a text string")
	}
	return kind, nil
}

// decodeHeader decodes a header into its type-specific struct. Unknown or
// duplicated keys are refused.
func decodeHeader(raw []byte, into any) error {
	if err := headerDecMode.Unmarshal(raw, into); err != nil {
		return fmt.Errorf("frame header is malformed: %w", err)
	}
	return nil
}
