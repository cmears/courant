package au.id.cmears.courant;

import android.location.Location;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.LinkedList;
import java.util.UUID;

/**
 * Created by chris on 7/07/15.
 */
public class Run {
    LinkedList<Segment> segments;
    UUID uuid;

    public Run() {
        uuid = UUID.randomUUID();
        segments = new LinkedList<Segment>();
        newSegment();
    }

    public void newLocation(Location loc) {
        segments.getLast().newLocation(loc);
    }

    public void newSegment() {
        segments.add(new Segment());
    }

    public int numSamples() {
        int n = 0;
        for (Segment s : segments) {
            n += s.numSamples();
        }
        return n;
    }

    public JSONObject toJSON() {
        JSONObject json = new JSONObject();
        try {
            json.put("uuid", uuid.toString());
            JSONArray segmentArray = new JSONArray();
            for (Segment s : segments)
                segmentArray.put(s.toJSON());
            json.put("segments", segmentArray);
            return json;
        } catch (JSONException e) {
            return null;
        }
    }

    public Run(JSONObject json) {
        try {
            String uuidS = json.getString("uuid");
            uuid = UUID.fromString(uuidS);
            JSONArray segmentArray = json.getJSONArray("segments");
            segments = new LinkedList<Segment>();
            for (int i = 0; i < segmentArray.length(); i++) {
                segments.add(new Segment(segmentArray.getJSONObject(i)));
            }
        } catch (JSONException e) {
            return;
        }
    }

    public String uuidString() {
        return uuid.toString();
    }
}

class Segment {
    LinkedList<Sample> samples;

    public Segment() {
        samples = new LinkedList<Sample>();
    }

    public void newLocation(Location loc) {
        samples.add(new Sample(loc));
    }

    public int numSamples() {
        return samples.size();
    }

    public JSONObject toJSON() {
        JSONObject json = new JSONObject();
        try {
            JSONArray sampleArray = new JSONArray();
            for (Sample s : samples)
                sampleArray.put(s.toJSON());
            json.put("samples", sampleArray);
        } catch (Exception e) {
            return null;
        }
        return json;
    }

    public Segment(JSONObject json) {
        try {
            JSONArray sampleArray = json.getJSONArray("samples");
            LinkedList<Sample> samples = new LinkedList<Sample>();
            for (int i = 0 ; i < sampleArray.length() ; i++) {
                samples.add(new Sample(sampleArray.getJSONObject(i)));
            }
        } catch (JSONException e) {
            return;
        }

    }
}

class Sample {
    Location loc;

    public Sample(Location _loc) {
        loc = _loc;
    }

    public JSONObject toJSON() {
        JSONObject json = new JSONObject();
        try {
            json.put("latitude", loc.getLatitude());
            json.put("longitude", loc.getLongitude());
            if (loc.hasAltitude()) json.put("altitude", loc.getAltitude());
            if (loc.hasBearing()) json.put("bearing", loc.getBearing());
            if (loc.hasAccuracy()) json.put("accuracy", loc.getAccuracy());
            if (loc.hasSpeed()) json.put("speed", loc.getSpeed());
            json.put("time", loc.getTime());
            return json;
        } catch (Exception e) {
            return null;
        }
    }

    public Sample(JSONObject json) {
        Location l = new Location("gps");
        try {
            l.setLatitude(json.getDouble("latitude"));
            l.setLongitude(json.getDouble("longitude"));
            l.setTime(json.getInt("time"));
            if (json.has("altitude")) l.setAltitude(json.getDouble("altitude"));
            if (json.has("bearing")) l.setBearing((float)json.getDouble("bearing"));
            if (json.has("accuracy")) l.setAccuracy((float)json.getDouble("accuracy"));
            if (json.has("speed")) l.setSpeed((float)json.getDouble("speed"));

        } catch (JSONException e) {
            ;
        }


    }
}
