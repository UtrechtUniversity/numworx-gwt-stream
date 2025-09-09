package nl.numworx.streamgwt.client;

/**
 * Looks like Consumer.
 * @see java.util.function.Consumer
 * @author velth101
 *
 */
@FunctionalInterface
public interface Consume {
	public void accept(String data);
}
