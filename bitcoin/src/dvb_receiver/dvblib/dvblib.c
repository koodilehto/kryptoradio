/*! \file dvblib.c
 * \brief A very simple DVB library.
 * \author Tuomas Virtanen
 * \copyright MIT
 * \date 2014
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <linux/dvb/frontend.h>
#include <linux/dvb/dmx.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>

#include "dvblib.h"

const char* dvb_get_error(dvb_device *dev) {
    return dev->error;
}

int dvb_open(dvb_device *dev, int dev_id, int frontend_id, int demuxer_id) {
    struct dvb_frontend_info info;
    char name_frontend[32];
    char name_demuxer[32];

    // Devices should go from 0 to 63, so limit IDs
    // this should also keep our name buffers from overflowing :)
    if(dev_id >= 64) {
        sprintf(dev->error, "Device ID is too high: %d", dev_id);
        return 1;
    }
    if(frontend_id >= 64) {
        sprintf(dev->error, "Frontend ID is too high: %d", dev_id);
        return 1;
    }
    if(demuxer_id >= 64) {
        sprintf(dev->error, "Demuxer ID is too high: %d", dev_id);
        return 1;
    }

    // Make device strings
    sprintf(name_frontend, "/dev/dvb/adapter%d/frontend%d", dev_id, frontend_id);
    sprintf(name_demuxer, "/dev/dvb/adapter%d/demux%d", dev_id, demuxer_id);

    // Clear out everything
    dev->fd_frontend = -1;
    dev->fd_demuxer = -1;
    dev->type = -1;
    dev->error[0] = 0;

    // Open frontend
    if((dev->fd_frontend = open(name_frontend, O_RDWR)) < 0) {
        sprintf(
            dev->error, 
            "Could not open frontend device %s: %s",
            name_frontend,
            strerror(errno));
        goto error_0;
    }

    // Open demuxer
    if((dev->fd_demuxer = open(name_demuxer, O_RDWR)) < 0) {
        sprintf(
            dev->error,
            "Could not open demuxer device %s: %s",
            name_demuxer,
            strerror(errno));
        goto error_1;
    }

    // Get device information
    if(ioctl(dev->fd_frontend, FE_GET_INFO, &info) < 0) {
        sprintf(
            dev->error,
            "Could not get device information: %s",
            strerror(errno));
        goto error_2;
    }

    // Check caps :)
    if(info.caps == FE_IS_STUPID) {
        sprintf(dev->error, "DVB device is too stupid :( Sorry.");
        goto error_2;
    }
    if(!(info.caps & FE_CAN_INVERSION_AUTO)) {
        sprintf(dev->error, "No INVERSION_AUTO");
        goto error_2;
    }
    if(!(info.caps & FE_CAN_FEC_AUTO)) {
        sprintf(dev->error, "No FEC_AUTO");
        goto error_2;
    }
    if(!(info.caps & FE_CAN_QAM_AUTO)) {
        sprintf(dev->error, "No QAM_AUTO");
        goto error_2;
    }
    if(!(info.caps & FE_CAN_TRANSMISSION_MODE_AUTO)) {
        sprintf(dev->error, "No TRANSMISSION_MODE_AUTO");
        goto error_2;
    }
    if(!(info.caps & FE_CAN_GUARD_INTERVAL_AUTO)) {
        sprintf(dev->error, "No GUARD_INTERVAL_AUTO");
        goto error_2;
    }

    // Copy information
    strcpy(dev->name, info.name);
    switch(info.type) {
        case FE_QPSK: dev->type = DVB_TYPE_SATELLITE;
        case FE_QAM: dev->type = DVB_TYPE_CABLE;
        case FE_OFDM: dev->type = DVB_TYPE_TERRESTIAL;
        default:
            dev->type = -1;
    }

    return 0;

error_2:
    close(dev->fd_demuxer);
error_1:
    close(dev->fd_frontend);
error_0:
    return 1;
}

int dvb_tune(dvb_device *dev, size_t frequency) {
    struct dvb_frontend_parameters params;    

    // Note! These parameters are only defined here for DVB-T, and should work
    // in Finland. In the future we might want to be able to set all of this
    // and read it all from channels.conf, but for now this is enough.
    params.frequency = frequency;
    params.inversion = INVERSION_AUTO;
    params.u.ofdm.bandwidth = BANDWIDTH_8_MHZ;
    params.u.ofdm.code_rate_HP = FEC_AUTO;
    params.u.ofdm.code_rate_LP = FEC_AUTO;
    params.u.ofdm.constellation = QAM_AUTO;
    params.u.ofdm.transmission_mode = TRANSMISSION_MODE_AUTO;
    params.u.ofdm.guard_interval = GUARD_INTERVAL_AUTO;
    params.u.ofdm.hierarchy_information = HIERARCHY_NONE;

    if(ioctl(dev->fd_frontend, FE_SET_FRONTEND, &params) < 0) {
        sprintf(
            dev->error,
            "Unable to tune: %s",
            strerror(errno));
        return 1;
    }

    return 0;
}

int dvb_get_status(dvb_device *dev, uint32_t *value) {
    fe_status_t status;
    *value = 0;
    if(ioctl(dev->fd_frontend, FE_READ_STATUS, &status) < 0) {
        sprintf(
            dev->error,
            "Unable to get device status: %s",
            strerror(errno));
        return 1;
    }

    if(status & FE_HAS_SIGNAL)
        *value |= DVB_STATUS_HAS_SIGNAL;
    if(status & FE_HAS_CARRIER)
        *value |= DVB_STATUS_HAS_CARRIER;
    if(status & FE_HAS_VITERBI)
        *value |= DVB_STATUS_HAS_VITERBI;
    if(status & FE_HAS_SYNC)
        *value |= DVB_STATUS_HAS_SYNC;
    if(status & FE_HAS_LOCK)
        *value |= DVB_STATUS_HAS_LOCK;
    if(status & FE_TIMEDOUT)
        *value |= DVB_STATUS_TIMEDOUT;
    if(status & FE_REINIT)
        *value |= DVB_STATUS_REINIT;

    return 0;
}

int dvb_get_ber(dvb_device *dev, uint32_t *value) {
    *value = 0;
    if(ioctl(dev->fd_frontend, FE_READ_BER, value) < 0) {
        sprintf(
            dev->error,
            "Unable to get BER for device: %s",
            strerror(errno));
        return 1;
    }
    return 0;
}

int dvb_get_snr(dvb_device *dev, int16_t *value) {
    *value = 0;
    if(ioctl(dev->fd_frontend, FE_READ_SNR, value) < 0) {
        sprintf(
            dev->error,
            "Unable to get SNR for device: %s",
            strerror(errno));
        return 1;
    }
    return 0;
}

int dvb_get_signal_strength(dvb_device *dev, int16_t *value) {
    *value = 0;
    if(ioctl(dev->fd_frontend, FE_READ_SIGNAL_STRENGTH, value) < 0) {
        sprintf(
            dev->error,
            "Unable to get signal strength for device: %s",
            strerror(errno));
        return 1;
    }
    return 0;
}

int dvb_get_uncorrected_blocks(dvb_device *dev, uint32_t *value) {
    *value = 0;
    if(ioctl(dev->fd_frontend, FE_READ_UNCORRECTED_BLOCKS, value) < 0) {
        sprintf(
            dev->error,
            "Unable to get uncorrected blocks for device: %s",
            strerror(errno));
        return 1;
    }
    return 0;
}

int dvb_set_buffer_size(dvb_device *dev, unsigned long size) {
    if(ioctl(dev->fd_demuxer, DMX_SET_BUFFER_SIZE, size) < 0) {
        sprintf(
            dev->error,
            "Unable to set buffer size: %s",
            strerror(errno));
        return 1;
    }
    return 0;
}

size_t dvb_read_stream(dvb_device *dev, void *buffer, size_t size) {
    size_t val = read(dev->fd_demuxer, buffer, size);
    if(val < 0) {
        sprintf(
            dev->error,
            "Unable to read data: %s",
            strerror(errno));

        if(errno == EOVERFLOW) return -2;
        if(errno == ETIMEDOUT) return -3;
    }
    return val;
}

int _dvb_map_stream_type_to_pes(int stream_type) {
    switch(stream_type) {
        case DVB_STREAM_VIDEO:
            return DMX_PES_VIDEO;
        case DVB_STREAM_AUDIO:
            return DMX_PES_AUDIO;
        case DVB_STREAM_TELETEXT:
            return DMX_PES_TELETEXT;
        case DVB_STREAM_SUBTITLE:
            return DMX_PES_SUBTITLE;
        case DVB_STREAM_PCR:
            return DMX_PES_PCR;
        default:
            return DMX_PES_OTHER;
    }
}

const char* dvb_get_stream_type_str(int stream_type) {
    switch(stream_type) {
        case DVB_STREAM_VIDEO:
            return "DVB_STREAM_VIDEO";
        case DVB_STREAM_AUDIO:
            return "DVB_STREAM_AUDIO";
        case DVB_STREAM_TELETEXT:
            return "DVB_STREAM_TELETEXT";
        case DVB_STREAM_SUBTITLE:
            return "DVB_STREAM_SUBTITLE";
        case DVB_STREAM_PCR:
            return "DVB_STREAM_PCR";
        default:
            return "DVB_STREAM_OTHER";
    }
}

int dvb_init_pes_stream(dvb_device *dev, int pid, int stream_type) {
    struct dmx_pes_filter_params pesfilter;

    // Select correct pid from frontend, pick the stream type we want, and
    // start listening to it immediately. Output should be TAP, so we can read
    // the stream.
    memset(&pesfilter, 0, sizeof(struct dmx_pes_filter_params));
    pesfilter.pid = pid;
    pesfilter.input = DMX_IN_FRONTEND;
    pesfilter.output = DMX_OUT_TAP;
    pesfilter.pes_type = _dvb_map_stream_type_to_pes(stream_type);
    pesfilter.flags = DMX_IMMEDIATE_START;

    // Tell the device to start listening.
    if(ioctl(dev->fd_demuxer, DMX_SET_PES_FILTER, &pesfilter) < 0) {
        sprintf(
            dev->error,
            "Error while trying to initialize PES filter stream: %s",
            strerror(errno));
        return 1;
    }
    return 0;
}

int dvb_init_section_stream(dvb_device *dev, int pid) {
    struct dmx_sct_filter_params sctfilter;

    // Just set PID and start listening.
    memset(&sctfilter, 0, sizeof(struct dmx_sct_filter_params));
    sctfilter.pid = pid;
    sctfilter.flags = DMX_IMMEDIATE_START;

    // Tell the device to start listening.
    if(ioctl(dev->fd_demuxer, DMX_SET_FILTER, &sctfilter) < 0) {
        sprintf(
            dev->error,
            "Error while trying to initialize section filter stream: %s",
            strerror(errno));
        return 1;
    }
    return 0;
}

int dvb_stop_stream(dvb_device *dev) {
    if(ioctl(dev->fd_demuxer, DMX_STOP) < 0) {
        sprintf(
            dev->error,
            "Error while trying to stop stream: %s",
            strerror(errno));
        return 1;
    }
    return 0;
}

const char* dvb_get_type_str(dvb_device *dev) {
    switch(dev->type) {
        case DVB_TYPE_TERRESTIAL:
            return "TERRESTIAL";
        case DVB_TYPE_SATELLITE:
            return "SATELLITE";
        case DVB_TYPE_CABLE:
            return "CABLE";
    }
    return "UNKNOWN";
}

void dvb_close(dvb_device *dev) {
    if(dev->fd_frontend >= 0) {
        close(dev->fd_frontend);
    }
    if(dev->fd_demuxer >= 0) {
        close(dev->fd_demuxer);
    }
}

