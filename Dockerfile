FROM erlang:20.3 as builder
MAINTAINER Radu Ciorba radu@devrandom.ro

ADD ./ /code

RUN useradd user -d /code && chown user:user -R /code
USER user
RUN cd /code && make

FROM erlang:20.3
RUN useradd erl --uid=1000 -d /rel

COPY --from=builder /code/_rel/feature_flag_proxy_release /rel
VOLUME /rel/log
RUN chown erl:erl -R /rel
USER erl

ENV FFP_CONFIG /rel/config.json

CMD /rel/bin/feature_flag_proxy_release foreground
