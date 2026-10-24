package mocknode

import (
	"bytes"
	"encoding/binary"
	"errors"
	"io"
	"net"

	gcbor "github.com/blinklabs-io/gouroboros/cbor"
)

const (
	segmentHeaderBytes = 8
	localTxMonitorID   = 9
	msgHasTx           = 7
)

// hfcConn makes the mock decode LocalTxMonitor's MsgHasTx as cardano-node
// does: [7, [era, txId]], the hard-fork-combinator transaction id. The stock
// gouroboros server decodes [7, txId], so each inbound MsgHasTx is rewritten
// to that form before the muxer sees it. A bare [7, txId] is what the node
// cannot decode, so the mock closes the connection on it, as the node does.
type hfcConn struct {
	net.Conn
	pending bytes.Buffer
	// eras records the era index of every MsgHasTx received.
	eras func(era uint64)
}

func (c *hfcConn) Read(buffer []byte) (int, error) {
	for c.pending.Len() == 0 {
		if err := c.readSegment(); err != nil {
			return 0, err
		}
	}
	return c.pending.Read(buffer)
}

func (c *hfcConn) readSegment() error {
	header := make([]byte, segmentHeaderBytes)
	if _, err := io.ReadFull(c.Conn, header); err != nil {
		return err
	}
	payload := make([]byte, binary.BigEndian.Uint16(header[6:8]))
	if _, err := io.ReadFull(c.Conn, payload); err != nil {
		return err
	}
	if binary.BigEndian.Uint16(header[4:6]) == localTxMonitorID {
		rewritten, err := c.rewriteHasTx(payload)
		if err != nil {
			_ = c.Conn.Close()
			return err
		}
		payload = rewritten
		binary.BigEndian.PutUint16(header[6:8], uint16(len(payload)))
	}
	c.pending.Write(header)
	c.pending.Write(payload)
	return nil
}

// rewriteHasTx rewrites every MsgHasTx among the segment's messages.
func (c *hfcConn) rewriteHasTx(payload []byte) ([]byte, error) {
	var out bytes.Buffer
	for rest := payload; len(rest) > 0; {
		var message []gcbor.RawMessage
		read, err := gcbor.Decode(rest, &message)
		if err != nil {
			return nil, err
		}
		raw := rest[:read]
		rest = rest[read:]
		var tag uint64
		if len(message) != 2 {
			out.Write(raw)
			continue
		}
		if _, err := gcbor.Decode(message[0], &tag); err != nil || tag != msgHasTx {
			out.Write(raw)
			continue
		}
		var id struct {
			gcbor.StructAsArray
			Era  uint64
			Hash []byte
		}
		if _, err := gcbor.Decode(message[1], &id); err != nil {
			return nil, errors.New("MsgHasTx does not carry a hard-fork transaction id")
		}
		if c.eras != nil {
			c.eras(id.Era)
		}
		encoded, err := gcbor.Encode([]any{msgHasTx, id.Hash})
		if err != nil {
			return nil, err
		}
		out.Write(encoded)
	}
	return out.Bytes(), nil
}
