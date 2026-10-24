package main

import (
	"net"
	"time"
)

// streamSegmentConn starts the muxer's segment read deadline at a segment's
// first byte. The muxer arms the deadline before reading a segment header,
// but an N2C peer may legitimately stay silent between segments (AwaitReply
// at the tip, or the client withholding credit). The rest of a started
// segment is still read under the muxer's bound. Only the muxer's single
// reader calls Read and SetReadDeadline; Close still interrupts an idle read.
type streamSegmentConn struct {
	net.Conn
	segmentTimeout  time.Duration
	awaitingSegment bool
}

func (c *streamSegmentConn) SetReadDeadline(deadline time.Time) error {
	c.awaitingSegment = !deadline.IsZero()
	c.segmentTimeout = time.Until(deadline)
	return c.Conn.SetReadDeadline(time.Time{})
}

func (c *streamSegmentConn) Read(buffer []byte) (int, error) {
	if !c.awaitingSegment || len(buffer) == 0 {
		return c.Conn.Read(buffer)
	}
	n, err := c.Conn.Read(buffer[:1])
	if n > 0 {
		c.awaitingSegment = false
		if deadlineErr := c.Conn.SetReadDeadline(time.Now().Add(c.segmentTimeout)); deadlineErr != nil && err == nil {
			err = deadlineErr
		}
	}
	return n, err
}
