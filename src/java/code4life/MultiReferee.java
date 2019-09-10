package code4life;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.StringReader;
import java.util.Map;
import java.util.Properties;

abstract class MultiReferee extends AbstractReferee {
	private Properties properties;

	public MultiReferee(final InputStream is, final PrintStream out, final PrintStream err) throws IOException {
		super(is, out, err);
	}

	@Override
	protected final void handleInitInputForReferee(final int playerCount, final String[] init) throws InvalidFormatException {
		this.properties = new Properties();
		try {
			for (final String s : init) {
				this.properties.load(new StringReader(s));
			}
		} catch (final IOException e) {
		}
		initReferee(playerCount, this.properties);
		this.properties = getConfiguration();
	}

	abstract protected void initReferee(int playerCount, Properties prop) throws InvalidFormatException;

	abstract protected Properties getConfiguration();

	protected void appendDataToEnd(final PrintStream stream) throws IOException {
		stream.println(OutputCommand.UINPUT.format(this.properties.size()));
		for (final Map.Entry<Object, Object> t : this.properties.entrySet()) {
			stream.println(t.getKey() + "=" + t.getValue());
		}
	}
}
