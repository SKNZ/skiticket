package ultralight;

import skiticket.nfc.NfcException;

import javax.smartcardio.CardException;

/**
 * Utility functions for reading and writing the Ultralight card.
 * 
 * @author Tuomas Aura
 */
public class UltralightUtilities {

	java.io.PrintStream msgOut;
	UltralightCommands ul;

	public UltralightUtilities(UltralightCommands ul, java.io.PrintStream msgOut) {
		this.ul = ul;
		this.msgOut = msgOut;
	}

	protected void userMessage(String msg) {
		if (msgOut != null)
			msgOut.println(msg);
	}

	// Read data selected pages on the card to a byte array.
	public boolean readPages(int startPage, int numberOfPages,
			byte[] destination, int destinationStartByte) throws CardException {
		// We always read and write one 4-byte page at a time.
		// The address is the number 0...39 of the 4-byte page.
		for (int i = 0; i < numberOfPages; i++) {
			boolean status = ul.readBinary(startPage + i, destination,
					destinationStartByte + i * 4);
			
			if (!status) {
				userMessage("Failed reading page " + i + ".");
				return false;
			}
		}
		return true;
	}

	public byte[] readPage(int page) throws CardException, NfcException {
	    byte[] buffer = new byte[4];
		if ((!readPages(page, 1, buffer, 0))) {
			throw new NfcException("Can't read page");
		}
	    return buffer;
	}

	// Read entire card memory. Returns 64-byte card memory image.
	public byte[] readMemory() throws CardException {
		byte[] memory = new byte[192];
		readPages(0, 44, memory, 0);
		return memory;
	}

	// Dump card memory to output.
	public void printMemory(java.io.PrintStream out) throws CardException {
		byte[] memory = readMemory();
		if (memory == null)
			return;
		out.println("\n\nCard memory dump:");
		out.println("Page Hexadecimal    ASCII   Binary");
		out.println("---------------------------------------------------------------");
		for (int i = 0; i < 44; i++) {
			// Page number
			out.printf("%02d   ", i);
			// Hexadecimal
			for (int j = 0; j < 4; j++)
				out.printf("%02X ", memory[4 * i + j]);
			out.print("   ");
			// ASCII characters
			for (int j = 0; j < 4; j++) {
				char c = (char) memory[4 * i + j];
				if (Character.isISOControl(c) || c >= 128)
					out.print(".");
				else
					out.printf("%c", c);
			}
			out.print("    ");
			// Binary
			for (int j = 0; j < 4; j++) {
				for (int k = 0; k < 8; k++)
					out.print((((memory[4 * i + j] >> (7 - k)) % 2) == 0 ? 0
							: 1));
				out.print(" ");
			}
			out.println();
		}
		out.println("---------------------------------------------------------------");
		out.println();
	}

	// Write data from a byte array into selected pages on the card.
	public boolean writePages(byte[] srcBuffer, int srcPos,
			int startPage, int numberOfPages) throws CardException {
		boolean status;
		// We always read and write one 4-byte page at a time.
		// The address is the number 0...39 of the 4-byte page.
		for (int i = 0; i < numberOfPages; i++) {
			status = ul.writeBinary(startPage + i, srcBuffer, srcPos + 4
					* i);
			if (!status) {
				userMessage("Failed writing page " + i + ".");
				return false;
			}
		}
		return true;
	}

	// Write selected pages from a card memory image into the card.
	public boolean writeMemory(byte[] memoryImage, int startpage,
			int numberOfpages) throws CardException {
		return writePages(memoryImage, startpage * 4, startpage, numberOfpages);
	}

	// Zero a single page of the card. (Cannot be used for pages 0...3).
	public boolean erasePage(int pageNumber) throws CardException {
		byte[] zeroPage = { 0x00, 0x00, 0x00, 0x00 };
		boolean status = ul.writeBinary(pageNumber, zeroPage, 0);
		if (!status && msgOut != null)
			userMessage("Failed erasing page " + pageNumber + ".");
		return status;
	}

	// Zero all pages 4...39. Fails if any one of the pages is locked.
	public boolean eraseMemory() throws CardException {
		boolean status = false;
		// Pages 0..3 are read-only or write-once.
		for (int i = 4; i < 39; i++) {
			status = erasePage(i);
			if (!status)
				break;
		}
		return status;
	}

	// Lock a page (not a good idea when practicing)
	public boolean lockPage(int pageNumber) throws CardException {
		if (pageNumber < 4 || pageNumber > 39)
			throw new CardException("Page to lock must be between 4 and 39.");

		byte[] page2 = { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00 };
		// Set one bit to indicate which page needs to be locked:
		int bit = 1 << pageNumber;
		page2[2] = (byte) (bit & 0xFF);
		page2[3] = (byte) ((bit >> 8) & 0xFF);

		boolean status = ul.writeBinary(2, page2, 0);
		if (!status)
			userMessage("Failed locking page" + pageNumber + ".");
		return status;
	}

	// Authenticate card with given key
	public boolean authenticate(byte[] key) throws CardException {
		if (key.length == 1) {
			return false;
		}
		boolean status;
		
		status = ul.authenticate(key);
		
		if (!status)
			userMessage("Authentication failed.");
		return status;
	}

	public boolean setAuth0(int page) throws CardException {
		boolean status = ul.setAuth0(page);

		if (!status)
			userMessage("Setting AUTH0 failed.");
		return status;
	}

	public boolean setAuth1(boolean noRead) throws CardException {
		boolean status = ul.setAuth1(noRead);

		if (!status)
			userMessage("Setting AUTH1 failed.");
		return status;
	}

	public boolean changeKey(byte[] key) throws CardException {

		boolean status = ul.changeKey(key);
		if (!status)
			userMessage("Changing key failed.");
		return status;
	}
	

}
