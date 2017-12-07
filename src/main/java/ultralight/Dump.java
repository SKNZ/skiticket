package ultralight;

/**
 * Dump data in hexadecimal and other formats.
 *
 * @author Daniel Andrade
 */
public class Dump {

    /**
     * Hex dump of the argument.
     *
     * @param b a byte
     * @return hex dump of the byte
     */
    public static String hex(byte b) {
        return String.format("%02x", b);
    }

    /**
     * Hex dump a byte array. A space is added between bytes.
     *
     * @param a the byte array
     * @return the hex dump
     */
    public static String hex(byte[] a) {
        return hex(a, true);
    }

    /**
     * Hex dump a byte array with or without spaces in between bytes.
     *
     * @param a     the byte array
     * @param space <code>true</code> to include a space between values
     * @return the hexadecimal representation of the byte array
     */
    public static String hex(byte[] a, boolean space) {
        StringBuilder sb = new StringBuilder();

        if (space) {
            int x = 0;
            for (byte b : a) {
                x += 1;
                sb.append(hex(b).toUpperCase());
                if (x < 4)
                    sb.append(" ");
                if (x == 4) {
                    x = 0;
                    sb.append("\n");
                }
            }
            if (sb.length() > 0) {
                sb.deleteCharAt(sb.length() - 1);
            }
        } else {
            for (byte b : a) {
                sb.append(hex(b));
            }
        }

        return sb.toString();
    }

    /**
     * @param a byte
     * @return byte a as binary string
     */
    public static String binary(byte a) {
        String r = String.format("%8s", Integer.toBinaryString(a & 0xFF)).replace(' ', '0');
        return r;
    }


    public static byte[] IntToByteArray(int input) {
        byte[] r = new byte[4];
        r[3] = (byte) (input & 0xff);
        input >>= 8;
        r[2] = (byte) (input & 0xff);
        input >>= 8;
        r[1] = (byte) (input & 0xff);
        input >>= 8;
        r[0] = (byte) input;
        return r;
    }

}