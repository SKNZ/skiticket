package ultralight;

import javax.smartcardio.CardException;
import java.util.HashMap;

/**
 * Class for atomic MIFARE Ultralight read and write commands.
 *
 * @author Tuomas Aura
 */
public class UltralightCommands {

	protected CardReader reader;

	// Set true to ignore writes to page 2 and to emulate page 3 with page 15:
	// Set false to really write the OTP and lock bits (cannot be reset).
	public static final boolean safe = false;
	private HashMap<Integer, Integer> pageMap = new HashMap<Integer, Integer>();

	/**
	 * Constructor for the UltralightCommands class.
	 *
	 * @param reader
	 *            Initialized object of the class Scl01Reader.
	 */
	public UltralightCommands(CardReader reader) {
		this.reader = reader;
		this.makeMap();
	}

	private void makeMap() {
		// address page -> where to find it / where to write it
		pageMap.put(2, 36);
		pageMap.put(3, 37);
		pageMap.put(40, 38);
		pageMap.put(41, 39);
	}

	protected void checkArgs(int adr, byte[] buffer, int pos)
			throws CardException {

		if (adr < 0 || adr > 44)
			throw new CardException("Bug: Memory page must be 0...44. It was "
					+ adr + ".");
		if (buffer == null)
			throw new CardException("Bug: read or write buffer is null.");
		if (buffer.length < pos + 4)
			throw new CardException(
					"Bug: Buffer too short. Ultralight is read and written 4 bytes at a time.");
	}

	/**
	 * Read a page of binary data from the smart card.
	 *
	 * @param adr
	 *            Number of the smart card memory page to be read. (page in
	 *            MIFARE Ultralight 4 bytes.)
	 * @param dstBuffer
	 *            Destination buffer to which the data will be read from the
	 *            smart card.
	 * @param dstPos
	 *            Byte index in the destination buffer to which the data will be
	 *            written. The buffer must have enough space (4 or 16 bytes) for
	 *            the data.
	 * @return Returns true of the read was successful.
	 * @throws CardException
	 *             Thrown only on unexpected errors. Normal errors are reported
	 *             as false return value.
	 */
	public boolean readBinary(int adr, byte[] dstBuffer, int dstPos)
			throws CardException {
		checkArgs(adr, dstBuffer, dstPos);

		if (safe && pageMap.containsKey(adr)) {
			adr = pageMap.get(adr);
		}
		return reader.readCommand(adr, dstBuffer, dstPos);
		
		/*boolean ret = false;
		int readAdr = adr;
		if (safe && pageMap.containsKey(adr)) {
			readAdr = pageMap.get(adr);
		}

		ret = reader.readCommand(readAdr, dstBuffer, dstPos);
		
		if (adr == 2){
			
			byte[] page2Buf = new byte[4];
			ret = reader.readCommand(adr, page2Buf, 0);
			
			if (ret){
				dstBuffer[dstPos + 0] = page2Buf[0];
				dstBuffer[dstPos + 1] = page2Buf[1];
			}
				
		}		
		return ret;*/

	}

	/**
	 * Write 4 bytes from source buffer to a card memory page.
	 *
	 * @param adr
	 *            Number of the smart card memory page to be written. (Page in
	 *            MIFARE Ultralight is 4 bytes.)
	 * @param srcBuffer
	 *            Source buffer from which data will be written to the card.
	 * @param srcPos
	 *            Byte position in the source buffer from which data is read.
	 * @return Returns true if the write was successful.
	 * @throws CardException
	 *             Throws exception only on unexpected errors. Normal errors are
	 *             reported as false return value.
	 */
	public boolean writeBinary(int adr, byte[] srcBuffer, int srcPos)
			throws CardException {
		checkArgs(adr, srcBuffer, srcPos);

		// If safe mode is on and the page is mapped to somewhere else, get the
		// secondary write location. Also, emulate OTP by bitwise OR with current
		// data on the page.
		if (safe && pageMap.containsKey(adr)) {
			adr = pageMap.get(adr);
			// Whoever wrote this didn't take into account the monotonic counter
			// This ruins everything, include erasing memory
//			byte[] current = new byte[4];
//			this.readBinary(adr, current, 0);
//			for (int i = 0; i < 4; i++) {
//				srcBuffer[i] = (byte) ((int) srcBuffer[i] | (int) current[i]);
//			}
		}

		boolean status = reader.writeCommand(adr, srcBuffer, srcPos);
		return status;
	}

	/*
	 * Authenticates the card using the key
	 * @param 16 byte key which is used to authenticate the card
	 * @return Returns true if authentication successful, false otherwise
	 * @throws CardException
	 * 				Throws exception only on unexpected errors. Normal errors are
	 *             	reported as false return value.
	 */
	public boolean authenticate(byte[] key) throws CardException {
		return reader.authenticateCommand(key);
	}

	/*
	 * Change the authenticates key on the card
	 * @param 16 byte key which is to used for future authentication
	 * @return Returns true if key change successful, false otherwise
	 * @throws CardException
	 * 				Throws exception only on unexpected errors. Normal errors are
	 *             	reported as false return value.
	 */
	public boolean changeKey(byte[] key) throws CardException {
		return reader.writeKey(key);
	}

	/*
	 * Set the AUTH0 value to the parameter value.
	 *
	 * @param integer value of the protection starting page
	 *
	 * @return Returns true if authentication configuration is set successful,
	 * 				false otherwise
	 * @throws CardException
	 * 				Throws exception only on unexpected errors. Normal errors are
	 *             	reported as false return value.
	 */
	public boolean setAuth0(int page) throws CardException {
		return reader.setAuth0(page);
	}

	/*
	 * Set the AUTH1 value to the parameter value.
	 *
	 * @param boolean value true indicates the access to the pages require authentication
	 * to read and write, false indicates the read is open while writing requires
	 * authentication
	 *
	 * @return Returns true if authentication configuration is set successful,
	 * 				false otherwise
	 * @throws CardException
	 * 				Throws exception only on unexpected errors. Normal errors are
	 *             	reported as false return value.
	 */
	public boolean setAuth1(boolean noRead) throws CardException {
		return reader.setAuth1(noRead);
	}
}
